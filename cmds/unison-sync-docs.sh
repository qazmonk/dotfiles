#!/bin/bash
# Script for syncing my documents folder with my server

unison /home/nate/ ssh://server/ -path Documents -logfile /home/nate/.unison.log  -repeat watch 
