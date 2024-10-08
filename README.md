Emulating [Helix Editor](https://docs.helix-editor.com) badly, by stripping down Evil mode to just the modes and adding some very simple interactive functions.

# Installation

You'll need to install Evil mode, but without requiring/importing the code. This package only uses the `evil-common` and `evil-core` modules from Evil. It sets up its own states. It also relies on [multiple-cursors.el](https://github.com/magnars/multiple-cursors.el), so install that too.

```elisp
(use-package helix
  :straight (helix :type git :host github :repo "tprost/helix"))
(require 'helix)
```

