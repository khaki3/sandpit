# jog (journal managed by git)
A journal tool managing by git.
If you want to use git's command-line tools,
you can use them on "~/.jog.d/".

## Requirements
* libgit2
* OpenSSL

## Usage
```
Usage: jog [--help] [COMMAND] [DATE]

COMMAND:
	open	open the journal
	show	show the journal
	help	show this usage

EXAMPLE:
	% jog	(open today's journal)
	% jog 1	(open tomorrow's journal)
	% jog show -1	(show yesterday's journal)
	% jog open 1	(open tomorrow's journal)
	% jog open 20141227	(open 2014/12/27's journal)
```
