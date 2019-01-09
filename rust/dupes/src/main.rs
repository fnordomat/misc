#![feature(bufreader_seek_relative)]
#![feature(bufreader_buffer)]
#![allow(non_snake_case)]

extern crate clap;
extern crate walkdir;
extern crate sha2;
extern crate regex;

use std::collections::{BTreeMap, BTreeSet};
use std::io;
use std::io::{BufRead, BufReader};
use std::path::Path;
use regex::Regex;
use clap::{Arg, App};
use walkdir::{WalkDir};
use sha2::{Sha256, Digest};

fn walk<P : AsRef<Path> + std::cmp::Eq + std::hash::Hash>
    (path : P,
     avoid_compare_if_larger_than : Option<u64>,
     ignore_sizes_below : Option<u64>,
     exclude_path_regex : Option<Regex>
    ) -> io::Result<()>
{
    let mut map : BTreeMap<u64, BTreeSet<_>> = BTreeMap::new();

    let isMatchOK = |s : &str|
    match &exclude_path_regex {
        Some(ex) => { !ex.is_match(&s) }
        None     => true
    };
    
    'walking: for entry in WalkDir::new(path).into_iter()
        .filter_entry (|e| (!e.file_type().is_dir())
         // Do not descend into paths excluded by -e
                || (e.path().to_str().map(&isMatchOK).unwrap_or(false)))
        .filter_map( Result::ok )
        .filter( |e| e.file_type().is_file() )
         // Do not consider files whose pathname is excluded by -e
        .filter( |e| e.path().to_str().map(&isMatchOK).unwrap_or(false) )
    {
        match entry.metadata() {
            Ok(m) => {
                let size = m.len();
                if let Some(s) = ignore_sizes_below {
                    if size < s {
                        continue 'walking;
                    }
                }
                map.entry(m.len()) . or_insert( BTreeSet::new() ) . insert(entry.into_path()) ;
            }
            Err(_e) => { }
        }
    }

    for (size, set) in map.iter() {
	if let Some(max_size) = &avoid_compare_if_larger_than {
            if size > max_size  {
                println!("{:} (avoiding disambiguation)", size);
	        for entry in set {
		    println!("  {:}", entry.display());
	        }
                continue;
            }
        }
        println!("{:}", size);
        if set.len() == 1 {
            for entry in set {
                println!("  {:}", entry.display());
            }
            continue;
        }
        else {
            let mut hashbins = BTreeMap::new();
            for entry in set {
                let maybe_f = std::fs::File::open(entry);
                if let Ok(f) = maybe_f {
                    let mut reader = BufReader::with_capacity(8192, f);
                    let mut hasher = Sha256::default();
                    'reading: loop {
                        let consumed = match reader.fill_buf() {
                            Ok(bytes) => {
                                hasher.input(bytes);
                                bytes.len()
                            },
                            Err(ref error) => {
                                println!("{:?} error reading file {:}", error.kind(), entry.display());
                                break 'reading;
                            }
                        };
                        reader.consume(consumed);
                        if consumed == 0 {
                            break 'reading;
                        }
                    }
                    let hashvec = hasher.result();
                    hashbins.entry(hashvec).or_insert(BTreeSet::new()).insert(entry);
                }
            }
            for (key, bin) in &hashbins {
                println!("  {:X}", key);
                for entry in bin {
                    println!("    {:}", entry.display());
                }
            }
        }
    }

    return Ok(());
}

fn main() {
    
    let matches = App::new("Dupes")
        .version("0.1.0")
        .author("fnordomat <GPG:46D46D1246803312401472B5A7427E237B7908CA>")
        .about("Finds duplicate files")
        .arg(Arg::with_name("dir")
             .short("d")
             .long("dir")
             .takes_value(true)
             .help("Base directory"))
        .arg(Arg::with_name("ignore_smaller_than")
             .short("i")
             .long("ignore-smaller-than")
             .takes_value(true)
             .help("Ignore all files smaller than given size (bytes)"))
        .arg(Arg::with_name("avoid_compare_if_larger_than")
             .short("a")
             .long("avoid-compare-if-larger")
             .takes_value(true)
             .help("Compare files of size >= X by size only"))
        .arg(Arg::with_name("exclude_path")
             .short("e")
             .long("exclude-path")
             .takes_value(true)
             .multiple(true)
             .help("Exclude part of path (glob)"))
        .get_matches();
    

    let mydir = matches.value_of("dir").unwrap_or(".");
    let ignore_sizes_below = matches.value_of("ignore_smaller_than").map_or(None, |x| x.parse::<u64>().ok());
    let exclude_exprs : Vec<&str> = matches.values_of("exclude_path").map_or([].to_vec(), |x| x.collect());
    
    let exclude_path_regex = 
        if exclude_exprs.is_empty()
           { None }
        else
           { Some(Regex::new(&exclude_exprs.join("|")).unwrap()) };
    
    let avoid_compare_if_larger_than : Option<u64> =
        matches.value_of("avoid_compare_if_larger_than").
        map_or(Some(1024*1024 * 32), |x| x.parse::<u64>().ok());

    let _ = walk(mydir, avoid_compare_if_larger_than, ignore_sizes_below, exclude_path_regex);
}
