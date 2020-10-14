# Haskell-Quine-McCluskey-Algorithm
Haskell implementation of the Quine–McCluskey algorithm I developed during my first year in university.

Usage: runghc qm.hs "INPUT_FILE" <br>
Input file syntax: on the first line list variables separated by spaces, then on
the following lines list the truth table. The truth table has to be complete and
has to have at least one minterm.
<br> <br>
Example: <br>
File "data.in":<br>
A B <br>
0 0 1 <br>
0 1 0 <br>
1 0 0 <br>
1 1 1 <br>
