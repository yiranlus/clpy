# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

from os.path import join, dirname, realpath, expandvars

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'CLPy'
copyright = '2025, Yiran Lu'
author = 'Yiran Lu'
release = '0.0.1'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    "sphinx.ext.intersphinx",
    "sphinxcontrib.cldomain",
    "sphinxcontrib.hyperspec",
]

templates_path = ['_templates']
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# -- CL Domain customizatoin -------------------------------------------------
cl_systems = [
    {
        "name": "clpy",
        "path": realpath(join(dirname(realpath(__file__)), "../")),
        "packages": [
            "clpy.bool"
        ]
    },
    {
        "name": "clthon",
        "path": realpath(join(dirname(realpath(__file__)), "../")),
        "packages": [
            "clthon"
        ]
    }
]

import sphinxcontrib.cldomain
cldomain_exe = join(dirname(realpath(sphinxcontrib.cldomain.__file__)), "cldomain.ros")
cl_custom_command = ["qlot", "exec", "ros", "-Q", "--", cldomain_exe]

#cl_quicklisp = join(dirname(realpath(__file__)), "../.qlot/quicklisp"),
highlight_language = "common-lisp"

cl_debug = True

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'
html_static_path = ['_static']
