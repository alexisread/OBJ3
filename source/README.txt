
                      README for OBJ3 2.09 Source Tree
     _________________________________________________________________

   Welcome to the OBJ3 source tree.

Source Structure

   The source code for the OBJ3 system is found in the [1]obj3 directory.
   In  particular,  note  that  the  standard OBJ3 prelude is included as
   [2]obj3/prelude/obj3sp.obj.

   The source code for the TRIM system is found in the [3]trim directory.
   The TRIM example code and documentation are found there as well.

   Many  OBJ3 examples are included in the [4]examples directory. See the
   [5]README there for more information.

   A few miscellaneous support programs (coded in C) are available in the
   [6]support directory.

Building

  Background

   The  OBJ3 system is written in Common Lisp. Modern Common Lisp systems
   can  compile Lisp code to C, thus a compiled implementation of OBJ3 is
   quite efficient.

   The  last  major release of OBJ3 (2.0) took place in April of 1992 and
   was  built  by Timothy Winkler, one of the primary OBJ3 developers. At
   that  time, OBJ3 built properly on several Common Lisp implementations
   including:
     * CMU  CL  - Carnegie Mellon's Common Lisp's last major release (17)
       from  CMU  took place in November, 1994. CMU-CL is currently under
       development  by  a  group of interested individuals and the latest
       version  (18e)  was made available in 2003. See [7]CMU Common Lisp
       Activities Page for more information.
     * LUCID  CL  (now Liquid CL from what used to be known as Harlequin,
       but  now  seems  to  be called Xanalys) - A commercial Common Lisp
       implementation  that  is  still  sold  and  supported.  The latest
       version  at the time of this writing is unknown, and it is unclear
       if  this  is  still  a  supported  product.  Xanalys also provides
       something  called "LispWorks Personal Edition"; we do not know how
       this relates to Lucid CL, nor have we tried to use it for OBJ3.
     * Kyoto  Common  Lisp (KCL) - Was the primary Lisp platform for OBJ3
       development   in   the   early  90s.  KCL  is  a  highly  portable
       implementation of Common Lisp. KCL evolved into AKCL (Austin Kyoto
       Common  Lisp) by Bill Schelter, which then evolved into GNU Common
       Lisp.  A  release of AKCL is available from [8]CMU but it is circa
       1992.  It  is  unclear  how  portable or supported AKCL is at this
       time.

   The  world  of  Common  Lisp implementations has changed over the last
   eight  years.  The primary free implementation that is widely used and
   available  today  is  GNU  Common  Lisp.  This release of OBJ3 is only
   guaranteed  to  run  under  GNU  CL (GCL) and CMU Common Lisp. You can
   obtain  GCL via the the [9]GCL home page; CMU Common Lisp is available
   via  the  [10]Cons.org  website.  Linux users can obtain references to
   various RPMs for GCL via [11]RPMfind.net's excellent auto-index.

   We are currently finishing a "port" of OBJ3 version 2 to CLISP 2.31.

   Thanks go primarily to [12]Sula Ma at Oxford for the OBJ3 2.04 port to
   GCL 2.2.2.

   We are interested in hearing if anyone ports OBJ3 to other Common Lisp
   implementations, in particular, LispWorks and [13]Franz Allegro CL.

   For  a  good  breakdown  of  modern  Common  Lisp implementations, see
   [14]this  informative  page.  All  other  questions  about Common Lisp
   implementation are probably addressed in the [15]Lisp FAQ.

  Building

   To  build  OBJ3,  you  need  GCL or CMU CL and GNU make. Set the shell
   variable 'lisp_compiler' to the executable of the CL compiler you wish
   to  use,  then type "make" in the OBJ3 root directory. After the build
   completes,  the  OBJ3  and  TRIM  executables will be found in the bin
   directory.

Questions or comments?

   Please email [16]obj-feedback@kindsoftware.com.
     _________________________________________________________________

      [ [17]Index ] [ [18]Readme ] [ [19]FAQ ] [ [20]Release Notes ] [
       [21]To-Do List ] [ [22]Bug List ] [ [23]Papers ] [ Source ] [
                     [24]Bibliography ] [ [25]License ]
     _________________________________________________________________

     [26]Best Viewed With Any Browser. [27]XHTML 1.0 Checked! [28]CSS,
                             Level 2 Checked! 


    by Joseph R. Kiniry <kiniry@acm.org>

   Last modified: Mon Sep 29 17:09:19 CEST 2003

References

   1. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/source/obj3/
   2. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/source/obj3/prelude/obj3sp.obj
   3. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/source/trim/
   4. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/source/examples/
   5. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/source/examples/README.txt
   6. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/source/support/
   7. http://www.cons.org/cmucl/
   8. http://www.cs.cmu.edu/afs/cs/user/mkant/Public/Lisp/impl/kcl/akcl/
   9. http://www.gnu.org/software/gcl/gcl.html
  10. http://www.cons.org/cmucl/
  11. http://www.rpmfind.net/linux/RPM/gcl.html
  12. mailto:sm@comlab.ox.ac.uk
  13. http://www.franz.com/
  14. http://www.elwoodcorp.com/alu/table/systems.htm
  15. http://www.faqs.org/faqs/lisp-faq/part1/preamble.html
  16. mailto:obj-feedback@kindsoftware.com
  17. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/index.html
  18. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/README.html
  19. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/FAQ.html
  20. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/RELEASE_NOTES.html
  21. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/TODO.html
  22. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/BUGS.html
  23. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/docs/index.html
  24. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/docs/obj_bib.html
  25. file://localhost/home/itt/kiniry/KindSoftware/sandbox/OBJ3/LICENSE.html
  26. http://www.anybrowser.org/campaign/
  27. http://validator.w3.org/check/referer
  28. http://jigsaw.w3.org/css-validator/check/referer
