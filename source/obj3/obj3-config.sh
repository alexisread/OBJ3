#!/bin/bash

#
# OBJ3 2.06,2.08,2.09 Copyright (c) 2000-2003 Joseph Kiniry, Joseph Goguen
# OBJ3 2.05 Copyright (c) 2000 Sula Ma, Joseph Kiniry, Joseph Goguen
# OBJ3 2.04 Copyright (c) 1988,1991,1993 SRI International
# TRIM Copyright (c) 1994,2001 Lutz Hamel
# All Rights Reserved
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
#   o Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# 
#   o Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# 
#   o Neither name of the Joseph Kiniry, Joseph Goguen, Sula Ma, or SRI
#   International, nor the names of its contributors may be used to
#   endorse or promote products derived from this software without
#   specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SRI
# INTERNATIONAL OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.

#
# $Id: obj3-config.sh,v 205.2.1.1 2003/09/23 14:18:01 kiniry Exp $
#

#
# Definitions of paths necessary for build.
#
# Todo: Should probably either 
# (a) factor this out and make it automagic, or
# (b) use autoconf
#

# The full path of this directory ("obj3").
obj3_lisp_dir=`pwd`

# The full path for the version of Common Lisp to use for compiling OBJ.
if [ ! "${lisp_compiler}" ]; then
    lisp_compiler=gcl
fi

# The full path for the version of Common Lisp to use for loading OBJ.
lisp_loader=$lisp_compiler
