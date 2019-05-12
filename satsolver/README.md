# satsolver

A very simple, easy to understand DPLL SAT solver in 7533 bytes of C

Input format: command line arguments are clauses. literals are a-z positive, A-Z negative.
Output format: sat (with a satisfying assignment), or unsat.

For example:

`./sat abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ ABCDEFz GHIJKLy MNOPQRx STUVwX PQRSTUvW KLUvwXY FGHIJOPQRu ABCDEFStZ KLOPQRsTU KOPQst KLrUVWX FGIJrxY KLQrUvW ABCDHIru GHOQrTUw KLPqSTU KLOqSuV BCDEPq GILMpQ LpqRSTU GOpqRSTuv ABDEpqRs KLOpqrSTUv GHIJKLMpqrsTU IJpqrst KLNoQ EHLopqR KLNopqr ABEGHn ABFHImO EFlOPQRS ABFIl AEFIkLT EFstUV JkLOstUv JkoQRstU NostuVWX stux ostu korst jOTUVWX jOPQRv juVWXZ jOPQRSuw ABCDEFNOsu jlrUVWx jklm ABcdefghj jopqtUvWx jrx jpqrs jkrst kost iTUvwx ghituV iJ ijK ijkL ijklM hOPQR hIJKL hOrst mnuVWX Opst ABcDeFgh hijopr hjOPr jKLMNOp joRSTu nOpqr noSTUVW hI gORSTU gu gry Ag fRUX fuZ fuz fOx fOrt pqrsT lOr`

