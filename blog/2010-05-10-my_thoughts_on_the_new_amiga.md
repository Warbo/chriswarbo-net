---
title: My thoughts on the new Amiga
---
I heard a while back that there's a new Amiga being worked on, and it <a href="http://www.osnews.com/story/23270/Interview_Trevor_Dickinson_A-EON_Technology">cropped up</a> in my browsing today too. I thought it would be worth espousing my opinions on this.

The new machine is, supposedly:

<ul>
 <li>

Dual-core 1.8GHz PowerISA v2.04+ CPU.

 </li>
 <li>

"Xena" 500MHz XMOS XS1-L1 128 SDS.

 </li>
 <li>

ATI Radeon R700 graphics card.

 </li>
 <li>

2GB RAM.

 </li>
 <li>

500GB Hard drive.

 </li>
 <li>

22x DVD combo drive.

 </li>
 <li>

Customised case, keyboard and mouse.

 </li>
 <li>

7.1 channel HD audio.

 </li>
 <li>

Ports and connectors:

  <ul>
   <li>

4x DDR2 RAM slots.

   </li>
   <li>

10x USB 2.0.

   </li>
   <li>

1x Gigabit Ethernet.

   </li>
   <li>

2x PCIe x16 slots (1x16 or 2x8).

   </li>
   <li>

2x PCIe x1 slots.

   </li>
   <li>

1x Xorro slot.

   </li>
   <li>

2x PCI legacy slots.

   </li>
   <li>

2x RS232.

   </li>
   <li>

4x SATA 2 connectors.

   </li>
   <li>

1x IDE connector.

   </li>
   <li>

JTAG connector.

   </li>
   <li>

1x Compact Flash.

   </li>
  </ul>
 </li>
</ul>

Now, straight away I can say that most of this is marketing bollocks because it's commodity stuff that can be picked up from a skip somewhere. What matters is the following:

<ul>
 <li>

Dual-core 1.8GHz PowerISA v2.04+ CPU.

 </li>
 <li>

"Xena" 500MHz XMOS XS1-L1 128 SDS.

 </li>

 <li>

Ports and connectors:

  <ul>
   <li>

1x Xorro slot.

   </li>
   <li>

JTAG connector.

   </li>
  </ul>
 </li>
</ul>

Now, these are what sets this computer apart from others. The processor may well be a Cell, which would be great for programmers but we'll have to wait and see (remember: there are NO games out there that would use this; only intensive, custom-written code would benefit). The JTAG connector is nice for low-level and hardware debugging, whilst the XMOS and 'Xorro' make reprogrammable hardware useful. So what may it be used for?

The idea that one would buy a computer with integrated FPGA for developing embedded FPGA systems is optimistic to say the least. A PCI card with an FPGA is a much better investment, since it's cheaper, can be replaced when newer chips come out and can be put into any computer from the last 15 years (or PCIe if you're really wanting speed, at the sacrifice of capable computers). Development for external devices is thus out, so the existence of this chip is only of use to the machine itself.

Now, what can the machine do, given this chip on board? Well, right away we can say nothing: since the chip is accessible via the Xorro slot, requiring a loopback connector to something like the PCI ports in order to actually use the chip for anything. The requirement to install such a loopback negates a lot of the advantage of building it into the machine (ie. that it's guaranteed to be there and accessible). So what does that leave us with? Well, if you're not a programmer, nothing, since you'll have to wait for others to build stuff for it and that isn't going to happen, so you'll need to get out of your Microsoft/Apple bubble and learn how your computer actually works for a change.

Now, if you are a programmer then you could rewrite the chip to, for example, offload intensive computation. This would, like the (potential) Cell, only be of extremely limited use. For a comparison, just look at the amount of software that uses GPUs to offload intensive tasks: there's hardly any, except for a few graphics apps, and GPUs are everywhere. The big winners from General Purpose GPU programming at the moment are researchers wanting more speed in their similations. The reason for this is that once the code has been written, it is immediately useful. There's no requirement for support software like GUIs to be made, there's no need to create specific data like graphics, 3D models, etc. like there is in games development, there's no need to market or deliver the code to others. There's no need to make it general enough for large numbers of computers. Once it's been written and is working on your GPU, then give it some numbers to crunch and get back the results. Done. Once again I'll state it: if you're not a programmer, this is of no use to you.

So what could we use an FPGA for, really? I would like to see FPGAs become integrated alongside CPUs/GPUs as co-processors, such that often-used programs (eg. Web browsers) can be compiled to the FPGA and run much faster. This would require a paradigm shift in compiler technology which puts it a long way off as yet, but in any case this isn't a good start since, as I said earlier, the FPGA isn't connected to the rest of the machine in a closed loop! When we consider that those experimenting with FPGA compilers, high-level languages, static analysis and such are often releasing their code as Open Source, then we can be rather certain that a Linux box would do much better than an AmigaOS box, and that if Linux is the target then any off-the-shelf/out-of-the-skip box will do, given a PCI FPGA card.

tl;dr: All it does is integrate (badly) existing technology, in a way that won't appeal to developers and doesn't have any point for "consumers" (a word I am defining to mean those too lazy to learn how to read and write).
