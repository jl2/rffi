# Build rffi

rffi: manifest.txt *.lisp *.asd
	buildapp --output rffi \
             --manifest-file ~/src/lisp/rffi/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system rffi\
             --entry 'rffi:main'

test: t/*.lisp *.lisp *.asd
	sbcl --eval "(ql:quickload :rffi.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "~/src/lisp/rffi/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
