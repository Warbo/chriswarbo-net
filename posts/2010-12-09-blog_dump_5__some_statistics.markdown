---
title: Blog Dump 5: Some Statistics
---
Statistical models are well suited to computer simulations. The most interesting results can usually be found in random processes, so here's an example showing the St Petersburg Lottery:

~~~python
#!/usr/bin/env python

import random

def flip():
    """Simulate a coin flip."""
    if random.randint(0,1): # Choose this statement or the next with equal chance
        return "Heads" # Give a "Heads"
    else:
        return "Tails" # Give a "Tails"

def lottery():
    """Run a St Petersburg Lottery, flipping a coin until it's tails. Each time it's heads
    our pot is doubled and added to the running total."""
    winnings = 1 # Start with 1 pound
    pot = 1 # The pot starts at 1 pound

    while flip() == "Heads": # Loop until we get a Tails
        pot = pot * 2 # Double the pot
        winnings = winnings + pot # Add it to our winnings

    return winnings # When we've finished looping, return the result

def play(rounds):
    """Run the lottery 'rounds' times in a row."""
    cost_per_game = 2 # Deduct this from the wealth for each round
    wealth = 1 # Start with 1 pound

    print "Rounds, Wealth" # Column headings for our spreadsheet
    print "0,1" # Print the first results

    for x in range(rounds): # Loop through each round
        wealth = wealth - cost_per_game + lottery() # Deduct the cost and add the winnings to our wealth
        print str(x+1)+', '+str(wealth) # Print the round number and our current wealth

play(1000) # Play 1000 rounds
~~~

The scenario is that we start with £1 and pay a fixed sum (here £2) to play a lottery, in which a coin if tossed. If it's tails then we take the winnings, which start at £1, but if it's heads then the winnings go up by twice the last increase, so they become £1 + £2. If we get a tails next, we get the winnings, but if we get heads it goes up by double the last increase again, so the winnings become £1 + £2 + £4. This keeps going on until we get a tails, when we get the winnings so far. The remarkable thing about this game is that the amount you can expect to win is infinite (0.5*£1 + 0.25*£2 + 0.125*£4+... = 0.5 + 0.5 + 0.5 + ...), so that you should be willing to pay any finite amount to enter. In this case the entrance fee for the lottery is £2, but it could be £2,000,000 and it wouldn't make a difference. We can see this from the graph below, where we run 1000 lotteries in a row, with 25 of these simulations running simultaneously (ie. 25,000 lotteries). The number of lotteries entered goes along the bottom, whilst the winnings goes up the side. The 25 runs of the simulation have been superimposed on each other to better approximate what's going on (I can't be arsed working out the error bars for a blog post). At any point on the graph, the darkness is a rough estimation of the probability of owning this amount of money after this many rounds (we use the area under the curve since, if we have £50 then we also have every amount below £50).

<img src="/images/lottery.png" alt="25 games of the St Petersburg Lottery, each with 1000 rounds" />

I've also made some simulations of the Monty Hall problem, random walks (intersecting and non-intersecting) and a few other statistical phenomena. I might post them if I get time. They can be the source of pretty patterns :)
