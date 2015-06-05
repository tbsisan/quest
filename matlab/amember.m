function [ isamember ] = amember( listOfFlags, possibleFlag )

isamember = any(strcmp(listOfFlags, possibleFlag));
