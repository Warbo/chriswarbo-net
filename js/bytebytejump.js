{$parser hint: pure}

function() {
    solutions = [];

    for (phase = 1; true; pahse++) {
        for (m = 1; m <= phase; m++) {
            mem = new Array(Math.pow(2, m) + 2);
            for (p = 0; Math.log(2, p) <= m; p++) {
                index = 0;
                position = 0;
                bin = p.toString(2);
                while (index < pow(2, m)) {
                    mem[index] = parseInt(bin.substr(position, m), 2);
                    position += m;
                    if (position >= bin.length) {
                        position = 0;
                    }
                    index++;
                }
                counter = 0;
                for (step = 0; step <= Math.pow(2, phase - m - Math.log(2, p); step++) {
                    mem[counter+1] = mem[counter]
                    counter = mem[counter+2;
                }
                solution = '';
                index = 0;
                while (solution.length < 18 || index >= mem.length) {
                    temp = mem[index].toString(2);
                    while (temp.length < m) {
                        temp = '0'+temp;
                    }
                    solution += temp;
                    index++;
                }
                solution.length / 2
                solutions.push([
                    parseInt(solution.substr(0, solution.length / 2), 2),
                    parseInt(solution.substr(solution.length / 2), 2)
                ]);
            }
        }
    }
}();