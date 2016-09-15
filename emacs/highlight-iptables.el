(require 'generic-x)

(define-generic-mode 'iptables-mode
  '("#")
  '("-A" "-j" "!" "-i" "-o" "-s" "-d" "-p" "--mark" "--save-mark" "--set-mark" "-m" "--ctstate" "--icmp-type" "--dport" "--tproxy-mark"
    "--on-port"
    "--on-ip"
    "--uid-owner" "--gid-owner"
    "--to-ports" "--to-source" "--to-destination"
    "--state" "--reject-with"
    "--limit" "--limit-burst" "--log-prefix"
    )
  '(;("*" . 'font-lock-operator)
    (":" . 'font-lock-operator)
    ("*nat" . 'font-lock-warning-face)
    ("*filter" . 'font-lock-warning-face)
    ("*mangle" . 'font-lock-warning-face)
    ("*raw" . 'font-lock-warning-face)
    ("COMMIT" . 'font-lock-function-name-face)
    ("INPUT" . 'font-lock-function-name-face)
    ("FORWARD" . 'font-lock-function-name-face)
    ("OUTPUT" . 'font-lock-function-name-face)
    ("PREROUTING" . 'font-lock-function-name-face)
    ("POSTROUTING" . 'font-lock-function-name-face)
    ;
    ("ACCEPT" . 'font-lock-constant-face)
    ("DROP" . 'font-lock-constant-face)
    ("LOG" . 'font-lock-constant-face)
    ("TPROXY" . 'font-lock-constant-face)
    ("RETURN" . 'font-lock-constant-face)
    ("REJECT" . 'font-lock-constant-face)
    ("REDIRECT" . 'font-lock-constant-face)
    ("CONNMARK" . 'font-lock-constant-face)
    ;
    ("RELATED" . 'font-lock-doc-face)
    ("NEW" . 'font-lock-doc-face)
    ("ESTABLISHED" . 'font-lock-doc-face)
    ("INVALID" . 'font-lock-doc-face)
    )
  '("\\.rules$|rules\\.save|rules-save")
  nil
  "My IPTABLES highlighting mode"
  )
