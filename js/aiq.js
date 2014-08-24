bf = function(symbols, orig_prog, input, output) {
    // Create a Brainfuck machine with the given program
    var pointer = 0;

    var read, write, random;
    (function(syms) {
        var tape    = {};
        read        = function( ) { return tape[pointer] || 0; };
        write       = function(x) { while (x < 0) x += syms;
                                    tape[pointer] = x % syms; };
        random      = function( ) { return Math.floor(Math.random() * syms); };
    }(symbols));

    var loops   = 0;
    var counter = 0;

    // We switch between inst and loop 'modes'
    var mode, inst, loop;

    // Regular mode. Pops instructions and runs them.
    inst = function(i) {
        var length = 100000;
        ({'>': function() { pointer = (pointer + 1) % length; },
          '<': function() { pointer = (pointer - 1) % length; },
          '+': function() { write(read() + 1); },
          '-': function() { write(read() - 1); },
          '.': function() { output(read()); },
          ',': function() { write(input()); },
          '%': function() { write(random()); },
          '[': function() { loops++; mode = loop; },
         })[i]();
        if (loops === 0) prog = prog.slice(1);
    };

    // Loop mode. 'Buffers' instructions and copies them to prog.
    loop = (function() {
        return function(i) {
            // Ignore nested loops
            if (i === '[') loops++;
            if (i === ']') loops--;

            // Are we at the end of the outermost loop?
            if (loops === 0) {
                // Remove the loop if our head reads 0
                if (read() === 0) prog = prog.slice(counter + 1);
                // Otherwise prefix the prog with the body
                else prog = prog.slice(1, counter) + prog;

                // Jump to the start of prog (-1, since we will ++)
                counter = -1;

                // Switch to regular mode
                mode = inst;
            }

            // Buffer instructions by shifting the program position
            counter++;
        };
    }());

    // Run
    return function() {
        mode    = inst;
        prog    = orig_prog;
        counter = 0;
        for (var steps = 0; steps < 1000; ++steps) {
            if (counter >= prog.length) return {success: false,
                                                steps:   steps};
            mode(prog[counter]);
        }
        return {success: true,
                steps:   steps};
    }
};
id = function(x) { return x; };
bf_prog = function() {
    // We only bother generating programs which can perform IO
    var i       = false;
    var o       = false;
    var loops   = 0;
    var prog    = '';
    var done    = false;
    var options = [
        function() {                return '+'; },
        function() {                return '-'; },
        function() {                return '<'; },
        function() {                return '>'; },
        function() { o = true;      return '.'; },
        function() { i = true;      return ','; },
        function() {                return '%'; },
        function() { if (loops > 0) {
                         loops--;   return ']';
                     }
                     done = i && o; return ''; },
        function() { loops++;       return '['; },
    ];
    while (!done) prog += (options[Math.floor(Math.random() * 8.9)])();
    return prog;
};

bf_length = function(prog) {
    // Programs can be encoded in prefix-free ternary as follows:
    //
    //        +--- [
    //        |
    //    +---+--- ] (or End of File)
    //    |   |
    //    |   +--- >
    //    |
    //    |   +--- <
    //    |   |
    // ---+---+--- +
    //    |   |
    //    |   +--- -
    //    |
    //    |   +--- .
    //    |   |
    //    +---+--- ,
    //        |
    //        +--- %

    // Since this is a balanced tree of depth 2, each character (including EOF)
    // encodes exactly two trits of information
    return (prog.length + 1) * 2;
};
