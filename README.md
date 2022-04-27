bfc -- a brainfuck compiler
===========================

bfc is a simple brainfuck compiler.  It's written in Haskell because I
hate myself and outputs QBE because why not.

Usage:

	$ bfc sources... > program.ssa
	$ qbe program.ssa > program.S
	$ cc -o program program.S && ./prog

bfc reads from standard input if no arguments are given.

bfc is release into the public domain.
