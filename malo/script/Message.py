#!/usr/bin/env python

"""
Name: Message.py
Purpose: G3Message prints the error message to stderr, including its severity level, and prints the stack of calls
Author:  Leonid Petrov. First version: 2009.01.30
$Revision: 1.4 $
"""

# ************************************************************************
# *                                                                      *
# *   Routine Message  prints a message to stderr in the form            *
# *   severity:  message                                                 *
# *   module_name(line)[routine1(line),routine2(line)...]                *
# *   where the severity is severity level, one of                       *
# *   INFO, WARNING, ERROR, FATAL_ERROR,                                 *
# *   modeul_name -- name of the python file containing the module,      *
# *   routine1 -- name of the first routine from the stack of calls,     *
# *   line -- is the line number of the call of Message;                 *
# *   routineN -- name of the N-1 throutine from the stack of calls,     *
# *   line -- is the line number of the call of that routine.            *
# *                                                                      *
# *                                                                      *
# * ____________________________ Parameters: ___________________________ *  
# *                                                                      *
# *  level   ( string, in  ) -- Severity level. Supported severity       *
# *                             levels:                                  *
# *                             INFO            shortcut: I              *
# *                             INFO-1          shortcut: I1             *
# *                             INFO-2          shortcut: I2             *
# *                             INFO-3          shortcut: I3             *
# *                             WARNING         shortcut: W              *
# *                             ERROR           shortcut: E              *
# *                             FATAL_ERROR     shortcut: F              *
# *                                                                      *
# *  message ( string, in  ) -- message.                                 *
# *                                                                      *
# *  Example:                                                            *
# *                                                                      *
# *   Message ( 'INFO', "Regridding is applied" )                        *
# *                                                                      *
# *   Message ( "FATAL_ERROR", "The number of levels in %s -- %d" +      *
# *   "exceeded the limit %d" % ( n_lev, file, max_lev ) )               *
# *                                                                      *
# *                                                                      *
# *  ### 30-JAN-2009     Message   v1.2 (c)  L. Petrov  13-FEB-2013 ###  *
# *                                                                      *
# ************************************************************************

import inspect
import sys
import os

def Message ( level, message ):
    module_name = os.path.basename  ( inspect.stack()[1][1] )
    trace_list = ""
    if ( len(inspect.stack()) > 2 ):
         for i in range(len(inspect.stack())-2,0,-1):
             trace_list = trace_list + inspect.stack()[i][3] + "(" + str(inspect.stack()[i+1][2]) + ")," 
         trace_list = trace_list[0:len(trace_list)-1] 

    lin_last = "(" + str(inspect.stack()[1][2]) + ")"

    if ( level == "INFO" ):
         prefix = "INFO: " 
    elif ( level == "I" ):
         prefix = "INFO: " 
    elif ( level == "WARNING" ):
         prefix   = "WARNING: " 
    elif ( level == "W" ):
         prefix   = "WARNING: " 
    elif ( level == "INFO-1" ): 
          prefix  = "INFO-1: "
    elif ( level == "I1" ): 
          prefix  = "INFO-1: "
    elif ( level == "INFO-2" ): 
          prefix  = "INFO-2: "
    elif ( level == "I2" ): 
          prefix  = "INFO-2: "
    elif ( level == "INFO-3" ): 
          prefix  = "INFO-3: "
    elif ( level == "I3" ): 
          prefix  = "INFO-3: "
    elif ( level == "ERROR" ):
         prefix   = "ERROR: " 
    elif ( level == "E" ):
         prefix   = "ERROR: " 
    elif ( level == "FATAL_ERROR" ):
         prefix   = "FATAL_ERROR: "
    elif ( level == "F" ):
         prefix   = "FATAL_ERROR: "
    else:
         prefix = "ERROR "

    print ( prefix + message + " in " + module_name + lin_last + \
            "[" + trace_list + "]" , file=sys.stderr )

def Introduction ( program_revision ):
# ************************************************************************
# *                                                                      *
# *   Routine Introduction prints an introductionary message to stderr   *
# *   that contains a) the name of the module; b) the revision number;   *
# *   c) the local time when the module was called. The program revision *
# *   is a tuple of three elements, the second element being the         *
# *   revision number.                                                   *
# *                                                                      *
# ************************************************************************
    import inspect
    import sys
    import os
    import datetime

    date_str = datetime.datetime.now().strftime("%Y.%m.%d_%H:%M:%S")[0:19]
    module_name = os.path.basename  ( inspect.stack()[1][1] )
    if ( len(program_revision.split(" ")) > 1 ): 
         str = "INFO: " + module_name + " Revision " + \
               program_revision.split(" ")[1] + " started on " + date_str
    else:
         str = "INFO: " + module_name + " Revision unknown" + \
         " started on " + date_str
    delim = "%s" % "=" * (len(str)-6)
    print ( str, file=sys.stderr )
    print ( "INFO: " + delim, file=sys.stderr )
