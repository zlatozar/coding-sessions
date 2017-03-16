#!/usr/bin/env groovy

// @Grab(group='books', module='etudes-for-programmers', version='1.0-SNAPSHOT')

import GameOfLife.Display

int[][] gliderFirstFrame = [[0, 1, 0], [0, 0, 1], [1, 1, 1]]
int[][] gliderSecondFrame = [[1, 0, 1], [0, 1, 1], [0, 1, 0]]
int[][] gliderThirdFrame = [[0, 0, 1], [1, 0, 1], [0, 1, 1]]

Display.display(gliderFirstFrame)
sleep(1000)
Display.display(gliderSecondFrame)
sleep(1000)
Display.display(gliderThirdFrame)