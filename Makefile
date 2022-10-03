# SPDX-License-Identifier: MIT

test:
	@emacs -batch -l tests/message-links-test.el -f ert-run-tests-batch-and-exit
