use strict;
use warnings;

sub main {
    my $quit = 0;
    my @tasks;
    my $lines = 0;

    # open tasks file and load them into array
    open(my $file, "+<", "tasks.txt") or die "Can't open file: tasks.txt";

    while (!eof($file)) {
        my $r = readline $file;
        @tasks[$lines++] = $r;
    }

    print $lines;

    # input loop
    while ($quit == 0) {
        print "[l]ist [a]dd [d]elete [e]xit\n>";
        my $ch = <STDIN>;
        chomp $ch;

        if ($ch eq 'l') {
            foreach my $i (0 .. $#tasks) {
                print "$i: $tasks[$i]";
            }
        }
        elsif ($ch eq 'a') {
            print "What task do you want to add to the todo list?\n>";
            my $task = <STDIN>;

            print $file "$task";

            @tasks[$lines++] = $task;
        }
        elsif ($ch eq 'd') {
            print "What task do you want to delete from the todo list?\n>";
            my $task_num = <STDIN>;
            chomp($task_num);
            if ($task_num < $lines) {
                print "Deleting task number $task_num\n";
                splice(@tasks, $task_num, 1);
                $lines--;
            }
        }
        elsif ($ch eq 'e') {
            print "Bye!\n";
            truncate($file, 0);
            foreach my $task (@tasks) {
                print $file "$task";
            }

            close($file);
            $quit = 1;
        }
    }
}

main();

