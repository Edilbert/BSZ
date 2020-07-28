# BSZ

Bit Shifter's Z Interpreter

This Z interpreter was programmed for the brand new MEGA65.

It lets you play famous Infocom Stories like ZORK, PLANETFALL,
THE HITCHHIKER'S GUIDE TO THE GALAXY, LEATHER GODDESSES OF PHOBOS
and many others on the new MEGA65 computer.

Just put the program file "z3-mega65.prg" on a D81 disk image
or a real 3,5" 720k floppy disk and add a story file.

The story file can be taken from other sources, e.g. PC images.
It must be a Infocom version 3 story file, often named with the
file extension ".z3", for example: "zork1.z3".
Or it may be any other Infocom compatible story file.
The name on the D81 image or floppy disk however MUST start
with the three characters "z3 " (unshifted z, 3 and a blank)
and it must be a sequential file (SEQ).

# Example for the disk content:
```
c1541 #8> dir
0 "zork-i          " bs 3d
26    "z3-mega65"         prg
335   "z3 zork-i"         seq
2799 blocks free.
c1541 #8>
```

# How to play

Get a copy of "zork1.pdf" from the internet for instructions.

Mount the disk image or insert the floppy disk.

Load and run the program "z3-mega65".

The program may be run in either native MEGA65 mode or in C64 mode.

If you want to assemble the source, use the assembler BSA,
available in the repository Edilbert/BSA.

Have fun!

