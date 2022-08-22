#!/bin/bash

(pidof $1 && killall $1) || ($* &)
