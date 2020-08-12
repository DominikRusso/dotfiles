# go N dirs up
function up
    set N $argv[1]
    # set N to 1 if argv[1] was empty
    set -q argv[1]; or set N 1

    if string match -qr '^[0-9]+$' $N
        for i in (seq $N)
            set path "$path../"
        end
        cd $path
    end
end

