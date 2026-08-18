#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Routine url_sanitizer checks whether a given srong is legitimate   *
# *   as a user input for htm,l scgo-bin script.                         *
# *   It returns True (the sting is ok) or False (the string did not     *
# *   pass sanitation test).                                             *
# *                                                                      *
# *   Supported parameters:                                              *
# *      "url"  -- checks whether a given string is a valid URL          *
# *      "dig"  -- checks whether a given string is a valid non-negative *
# *                number with an optiomal decimal dot.                  *
# *      "date" -- checks whether a given string is a valid date.        *
# *                                                                      *
# * Change: classified semicolon ; as inadmissble.                       *
# *                                                                      *
# * ### 07-JAN-2024  url_sanitizer.py v1.2 (c) L. Petrov 26-JUN-2026 ### *
# *                                                                      *
# ************************************************************************
from urllib.parse import unquote

dig_and_dot_set = set('0123456789.')
date_set = set('0123456789.:_-Tnow')
allowed_charset = set('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_&@./:+-=?%')

max_string_len = 1024

def url_sanitizer ( string, typ ):
    """
    Returns 0 if the url string is fine and 1 if it failed sanitation test
    """
    if ( len(string) > max_string_len ):
         chars_are_ok = False
         return chars_are_ok 

    if ( typ == "url" ):
         chars_are_ok = all( (temp_char in allowed_charset) for temp_char in string )
#
# ------ If a string contains | or ^ or $ or ` or ; (semicolon) or < or > or char(127) or blank
# ------ that string is considered "bad". A bad string fails the test
#
         if ( "%2E" in string or "%2e" in string ): # ^
              chars_are_ok = False
         if ( "%3C" in string or "%3c" in string ): # <
              chars_are_ok = False
         if ( "%3B" in string or "%3b" in string ): # ;
              chars_are_ok = False
         if ( "%3E" in string or "%3e" in string ): # >
              chars_are_ok = False
         if ( "%24" in string                    ): # $
              chars_are_ok = False
         if ( "%7C" in string or "%7c" in string ): # |
              chars_are_ok = False
         if ( "%7F" in string or "%7f" in string ): # 127
              chars_are_ok = False
         if ( "%60" in string                    ): # `
              chars_are_ok = False
#
# ------ If a string contains characters with the 8th bit on
# ------ we also set the string as bad.
# ------ Well, this may invalidate a string at languages other than English
#
         if ( "%8" in string or \
              "%9" in string or \
              "%a" in string or \
              "%A" in string or \
              "%b" in string or \
              "%B" in string or \
              "%c" in string or \
              "%C" in string or \
              "%d" in string or \
              "%D" in string or \
              "%e" in string or \
              "%E" in string or \
              "%f" in string or \
              "%F" in string    ): # Characters above 127
              chars_are_ok = False
         if ( "%0" in string or \
              "%1" in string    ):
              chars_are_ok = False

#
# ------ Let us inquite the string
#
         try:
             unq_string = unquote(string)
         except:
             unq_string = string

#
# ------ An htmnl tag <script atr=value> is not allowed
#
         if ( "<script"  in unq_string.lower() ):
              chars_are_ok = False
         if ( "<\\script" in unq_string.lower() ):
              chars_are_ok = False

         return chars_are_ok
    elif ( typ == "dig" ):
#
# ------ Check whether this character set has only digits and a decimal point
#
         chars_are_ok = all( (temp_char in dig_and_dot_set) for temp_char in string )
         return chars_are_ok
    elif ( typ == "date" ):
#
# ------ Check whether this strong is a valid date
#
         chars_are_ok = all( (temp_char in date_set) for temp_char in string )
         return chars_are_ok
    else:
         chars_are_ok = False
         return chars_are_ok 

