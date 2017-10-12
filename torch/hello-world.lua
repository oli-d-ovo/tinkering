local torch = require 'torch'

local cmd = torch.CmdLine()
cmd:text()
cmd:text('Options for my NN')
cmd:option('-units', 10,'units in the hidden layer')
cmd:option('-learningRate', 0.1, 'learning rate')
cmd:text()

local opt = cmd:parse(arg)
