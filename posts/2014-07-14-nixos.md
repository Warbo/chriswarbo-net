---
title: NixOS
author: Chris
---
## Introduction

During a long weekend in Birmingham I finally decided to bite the bullet and switch my laptop to NixOS.

The decision process followed the same format as most of my Free Software learning experiences:

 1. Try to do something simple (set up a Hakyll site)
 1. Fail (dependency hell for Hakyll in Trisquel and cabal)
 1. Go down a rabbit hole (NixOS) in an attempt to fix it

In fact I tried the standalone Nix package manager on Trisquel, which managed to install Hakyll properly, but due to its sandboxing the libraries weren't available to GHC itself. Rather than messing around with custom dev environments, I decided to go the whole hog and install NixOS itself.

## Installation

It took me a few attempts to boot into the live environment. The recommended route is to burn and boot from a CD, but my laptop doesn't have an optical drive. I also didn't have a USB drive handy.

I tried to hack the install script to work from Trisquel, but it required too many external utilities that I didn't have. The wiki's guide to booting from another Linux system was interesting, but didn't work due to some systemd error.

Eventually I took the slightly risky approach of giving a KVM instance access to my real hard drive, and installing from there:

    sudo kvm -cdrom /path/to/cd/image.iso -hda /dev/sda

This worked, since I'd already created and formatted the partition from within Trisquel, and made sure that no partition was ever mounted in Trisquel and KVM at the same time (since that would corrupt the disk).

## Configuration

Overall I found configuring NixOS to be pretty straightforward. There are enough configuration.nix files floating around the Web that mine is currently just copypasta. Here are some issues I ran into:

### Wifi

I tend to use wicd for managing my network connections. In order for this to work, we need to specifically disable the other networking options, like wpa_supplicant. Here's what eventually worked for me:

    networking = {
      hostName = "nixos"; # Define your hostname.
      interfaceMonitor.enable = false; # Disable in favour of wicd
      wireless.enable = false;  # Disable in favour of wicd
      useDHCP = false;  # Disable in favour of wicd
      wicd.enable = true;
    };

It's a little annoying that wicd comes with wicd-gtk and not wicd-curses, but that's pretty low on my priority list.

### X

X was a breeze to set up for my physical laptop, here's the config which includes XMonad:

    services.xserver = {
      enable = true;
      layout = "gb";
      xkbOptions = "ctrl:nocaps";
      windowManager.xmonad.enable = true;
      windowManager.default = "xmonad";
      windowManager.xmonad.enableContribAndExtras = true;
      displayManager = {
        auto = {
          enable = true;
          user = "chris"; # login as "chris"
        };
      };
    };

Running X in KVM required the following addition:

    services.xserver.videoDrivers = [ "cirrus" "vesa" "vga" ];

Also KVM had to be run with the "-vga std" commandline option.

## Conclusions

Overall I'm really impressed with NixOS. Not only did I get Hakyll up and running pretty quickly, but it was very quick to get back to my regular setup: XMonad, st, tmux, conkeror, cmus and emacs. This covers most of my needs, but I imagine I'll be installing things every now and again for a few months.
