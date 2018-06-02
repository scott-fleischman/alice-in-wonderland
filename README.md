# alice-in-wonderland
Textual analysis and Twitter bot

[![Build Status](https://travis-ci.org/scott-fleischman/alice-in-wonderland.svg?branch=master)](https://travis-ci.org/scott-fleischman/alice-in-wonderland)

## Output of Analysis
See the [output files](output/README.md) for basic textual analysis.

See the analysis code [here](tree/master/analysis).

## Twitter Bot
See the Twitter bot in action at [@PoorLittleAlice](https://www.twitter.com/poorlittlealice).

It tweets one sentence every half-hour. Sentences longer than 280 characters are broken into multiple tweets which are posted in a single thread. Poetry and other significant indentation is preserved when tweeting.

See the Twitter bot code [here](tree/master/twitter).
