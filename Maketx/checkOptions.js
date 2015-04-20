var fs = require('fs');
var readline = require('readline-sync');
var fname;
var answer;

function checkOptions(options) {
    if (!((options.send || options.issue) && !(options.send && options.issue))) { 
        console.log ('One operational option must be specified!');
        return 1;
    }
    
    if (!options.input_file) {
        console.log ('Input file is not specified!');
        return 2;
    }
    
    if (!fs.existsSync(options.input_file)) { 
        console.log ('Input file does not exist.');
        return 3;
    }
    
    if (!options.output_file) {
        console.log ('Output file is not specified');
        return 4;
    }

    while (fs.existsSync(options.output_file)) {
        answer = readline.question("File " +
                                   "\"" + options.output_file + "\"" +
                                   " already exists. Overwrite? (y / n)...");
        if (answer == 'y') {
            fname = readline.question("Enter filename: ");
            options.output_file = fname;
        }
        else if (answer == 'n')
            break;
    }    

    return 0;
}

module.exports = {
    checkOptions : checkOptions
}
