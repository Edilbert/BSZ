# BSZ

Bit Shifter's Z Interpreter

This Z interpreter was programmed for the brand new MEGA65.

It lets you play famous Infocom Stories like ZORK, PLANETFALL,
THE HITCHHIKER'S GUIDE TO THE GALAXY, LEATHER GODDESSES OF PHOBOS
and many others on the new MEGA65 computer.

Just put the program file "bsz-mega65" on a D81 disk image
or a real 3,5" 720k floppy disk and add a story file.

The story file can be taken from other sources, e.g. PC images.
It must be a Infocom version 3,4 or 5 story file, often named with the
file extension ".z3" or ".z5", for example: "zork1.z3".
Or it may be any other Infocom compatible story file.
The name of the story file on the D81 image or floppy disk however MUST start
with the character "z" (unshifted z)
and it must be a sequential file (SEQ).

# Example for the disk content:
```
c1541 #8> dir
0 "zork-i          " bs 3d
30    "bsz-mega65"        prg
335   "zork-i"            seq
2799 blocks free.
c1541 #8>
```

# How to play

Get a copy of "zork1.pdf" from the internet for instructions.

Mount the disk image or insert the floppy disk.

Load and run the program "bsz-mega65".

The program may be run in either native MEGA65 mode or in C64 mode.

If you want to assemble the source, use the assembler BSA,
available in the repository Edilbert/BSA.

Have fun!

