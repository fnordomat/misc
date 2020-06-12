clean:
	-rm tmp_*.c tinflat tinform toutform toutform_letter toutform_maze

doublecheck:
	ccache gcc -Wall -lm tinflat.c -otinflat
	ccache gcc -Wall -lm tinform.c -otinform
# block dogfood
	cat tinform.c | ./tinform - > tmp_outform.c
	cat tmp_outform.c | ./tinform - > tmp_outform2.c
	ccache gcc -lm tmp_outform.c -otoutform
	cat tinform.c | ./toutform - > tmp_outform3.c
	cat tmp_outform.c | ./toutform - > tmp_outform4.c
# block/+maze dogfood
	cat tinform.c | ./tinform - maze.pbm > tmp_maze_outform.c
	cat tmp_outform.c | ./tinform - maze.pbm > tmp_maze_outform2.c
	ccache gcc -lm tmp_maze_outform.c -otoutform_maze
	cat tinform.c | ./toutform_maze - maze.pbm > tmp_maze_outform3.c
	cat tmp_maze_outform.c | ./toutform_maze - maze.pbm > tmp_maze_outform4.c
# P4
	./tinform tinform.c letter.pbm > tmp_letter_outform.c
	./tinform tmp_letter_outform.c letter.pbm > tmp_letter_outform2.c
	ccache gcc -lm tmp_letter_outform.c -otoutform_letter
	./toutform_letter tinform.c letter.pbm > tmp_letter_outform3.c
# idempotent?:
	cat tinform.c | ./tinflat > tmp_tinform_flat.c
	cat tmp_outform.c | ./tinflat > tmp_outform_flat.c
	cat tmp_outform2.c | ./tinflat > tmp_outform2_flat.c
	cat tmp_outform3.c | ./tinflat > tmp_outform3_flat.c
	cat tmp_outform4.c | ./tinflat > tmp_outform4_flat.c
	cat tmp_maze_outform.c | ./tinflat > tmp_maze_outform_flat.c
	cat tmp_maze_outform2.c | ./tinflat > tmp_maze_outform2_flat.c
	cat tmp_maze_outform3.c | ./tinflat > tmp_maze_outform3_flat.c
	cat tmp_maze_outform4.c | ./tinflat > tmp_maze_outform4_flat.c
# hopefully enough
	cat tmp_letter_outform3.c | ./tinflat > tmp_letter_outform3_flat.c
# flatdiffs
	diff tmp_tinform_flat.c tmp_outform_flat.c
	diff tmp_outform_flat.c tmp_outform2_flat.c
	diff tmp_outform_flat.c tmp_outform3_flat.c
	diff tmp_outform_flat.c tmp_outform4_flat.c
	diff tmp_tinform_flat.c tmp_maze_outform_flat.c
	diff tmp_maze_outform_flat.c tmp_maze_outform2_flat.c
	diff tmp_maze_outform_flat.c tmp_maze_outform3_flat.c
	diff tmp_maze_outform_flat.c tmp_maze_outform4_flat.c
	diff tmp_letter_outform3_flat.c tmp_maze_outform4_flat.c
	diff tmp_outform.c tmp_outform2.c
	diff tmp_outform.c tmp_outform4.c
	diff tmp_outform.c tmp_outform3.c
# examples
	./tinform examples/0000.c > tmp_0000.c
	./tinform examples/0001.c > tmp_0001.c
	./tinform examples/0002.c > tmp_0002.c
	ccache gcc -lm tmp_0000.c -otmp_foo.o.c
	ccache gcc -lm tmp_0001.c -otmp_foo.o.c
	ccache gcc -lm tmp_0002.c -otmp_foo.o.c
	./tinform tmp_0000.c > tmp_0000_2.c
	./tinform tmp_0001.c > tmp_0001_2.c
	./tinform tmp_0002.c > tmp_0002_2.c
	diff tmp_0000.c tmp_0000_2.c
	diff tmp_0001.c tmp_0001_2.c
	diff tmp_0002.c tmp_0002_2.c
	echo "SUCCESS"
