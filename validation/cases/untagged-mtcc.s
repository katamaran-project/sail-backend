# MTCC written with a non-executable capability (the data root): tag cleared.
# A trap then installs the untagged MTCC as PCC; the fetch finds PCC and MTCC
# untagged and stops the model (`not_implemented`).
cspecialr ct0, mtdc
cspecialw mtcc, ct0
.word 0x00000000          # illegal: trap to the untagged MTCC
addi a2, zero, 99         # not reached
