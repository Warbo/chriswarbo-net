// This runs Levin Search for the BitBitJump language, see
// http://mazonka.com/bbj/
//
// BitBitJump acts on a bitstring, which can be thought of as a
// virtual machine's memory. It has a counter, initialised to
// zero, and it runs a single instruction over and over:
//
//  1) Read a binary number from memory, m bits long, starting
//     at the counter. Let's call this "A".
//  2) Read another binary number from memory, m bits long,
//     starting at (counter + m). Let's call this "B".
//  3) Copy the Ath bit of memory to Bth bit.
//  4) Read a binary number from memory, m bits long, starting
//     at (counter + 2m). Make this the new counter value.
//
// Numbers are big-endian, as they are in Mathematics, so the
// only implementation detail is the value of m. This is
// effectively the word-size for an m-bit BitBitJump machine.
// To be Turing Complete, the machine must be able to address
// an unbounded amount of memory, but this would require
// reading an unbounded number (m) of bits. If the first read
// is unbounded, then we will never get to step 2!
// In practice, the value of m should be similar to the
// word size of the hardware that's interpreting a virtual
// BitBitJump machine, or if it's BitBitJump hardware then it
// should be the log-base-2 of however much RAM can be
// squeezed in!
// In this implementation, we incorporate m into our Levin
// Search. m starts at 1, and if the length of the current
// program is more than m, we remove the first m bits,
// increment m and double the size of our memory.
// Thus we go from looping through phase(program(step)) to
// phase(machine(program(step)))

// This is a "Web Worker", which responds to message sends
onmessage = (function() {
  var run_prog = (function() {
    var mem, index, counter, step, stream, size;
    var result = {'x': 0, 'y':0};
    var read_address = function(start, length) {
      return parseInt(mem.slice(start, start+length).join(''), 2);
    }

    // Runs program p on machine m in phase phase
    // m is the size of a word
    return function(phase, m, p) {
      if (!p) {
        // The first program, we must set up its machine
        size = Math.pow(2, m)+2*m;
      }
      // Put p in the memory
      mem = p.toString(2).split('');
      mem.reverse();  // We'll always have a leading 1 otherwise!
      while (mem.length < size) {
        mem.push("0");
      }
      while (mem.length > size) {
        mem.pop();
      }

      // Run 2^(phase-m-length(p)) steps of p on m
      counter = 0;
      for (step=Math.pow(2, phase - m - Math.log(2, p)); step > 0; step--) {
        mem[read_address(counter, m)] = mem[read_address(counter+m, m)];
        counter = read_address(counter+2*m, m);
      }

      // Extract a coordinate from the start of the memory. Alternate bits
      // go into x and y, respectively, until they're both 9 bits.
      result['x'] = parseInt([mem[0],  mem[2],  mem[4],  mem[6], mem[8],
                              mem[10], mem[12], mem[14], mem[16]].join(''), 2);
      result['y'] = parseInt([mem[1], mem[3], mem[5], mem[7], mem[9],
                              mem[11], mem[13], mem[15], mem[17]].join(''), 2);

      result['phase'] = phase;
      result['m']     = m;
      result['p']     = p;

      postMessage(result);
    };
  })();

  return (function() {
    var phase = 4;
    var m     = 1;
    var p     = 0;
    return function(event) {
      run_prog(phase, m, p);
      p++;
      if (p > Math.pow(2, m)) {
        p = 0;
        m++;
        if (m > phase) {
          m = 1;
          phase++;
        }
      }
    };
  })();
})();
