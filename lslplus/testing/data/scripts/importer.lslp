// importer
integer dbg = TRUE;

$import debug.lslm(DEBUG=dbg); 
$import module1.lslm(DEBUG=dbg);

default {
    state_entry() {
        debug("Hello Scripter");
    }
}

