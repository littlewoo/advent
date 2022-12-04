
DAY_NO=$1
STRIPPED_DAY=$(expr $DAY_NO + 0)
DIR=./$DAY_NO
YEAR=${PWD##*/}

echo 'Fetching puzzle material for year '$YEAR', day '$DAY_NO'.'

mkdir $DIR
cp Main.hs $DIR
curl 'https://adventofcode.com/'$YEAR'/day/'$STRIPPED_DAY'/input' -H 'Cookie: session='$(cat ./session.txt) > $DIR/input
curl 'https://adventofcode.com/'$YEAR'/day/'$STRIPPED_DAY'' -H 'Cookie: session='$(cat ./session.txt) > $DIR/puzzle1.html

