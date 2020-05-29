**************************
Read the Docs Sphinx Theme
**************************

.. image:: https://img.shields.io/pypi/v/sphinx_rtd_theme.svg
   :target: https://pypi.python.org/pypi/sphinx_rtd_theme
   :alt: Pypi Version
.. image:: https://travis-ci.org/readthedocs/sphinx_rtd_theme.svg?branch=master
   :target: https://travis-ci.org/readthedocs/sphinx_rtd_theme
   :alt: Build Status
.. image:: https://img.shields.io/pypi/l/sphinx_rtd_theme.svg
   :target: https://pypi.python.org/pypi/sphinx_rtd_theme/
   :alt: License
.. image:: https://readthedocs.org/projects/sphinx-rtd-theme/badge/?version=latest
  :target: http://sphinx-rtd-theme.readthedocs.io/en/latest/?badge=latest
  :alt: Documentation Status

This Sphinx_ theme was designed to provide a great reader experience for
documentation users on both desktop and mobile devices. This theme is used
primarily on `Read the Docs`_ but can work with any Sphinx project. You can find
a working demo of the theme in the `theme documentation`_

.. _Sphinx: http://www.sphinx-doc.org
.. _Read the Docs: http://www.readthedocs.org
.. _theme documentation: https://sphinx-rtd-theme.readthedocs.io/en/latest/

Simple Installation & Usage:
----------------------------

We assume that you already have a sphinx documentation project (read their 
documentation for advice on how to do that). In order to use this theme, you 
can either install it in a central location (if you plan to use it for more 
then one project) or install a copy of it with your sphinx documentation 
project (recommended route, explained here). 

You can get the latest version of this theme as a unix 'tarball' `here
<https://github.com/OB7-IRD/sphinx_ob7_theme/tarball/master>`_, or if
you prefer, the latest version is available as a zip file `here
<https://github.com/OB7-IRD/sphinx_ob7_theme/zipball/master>`_.

Assuming you have extracted this theme under::

   <your sphinx project>/sphinx_ob7_theme

and your documentation sources live in a subdirectory e.g.::

   <your sphinx project>/source

then modify your project/source/conf.py to include the theme directory and
indicate that this theme should be used::

   html_theme = 'sphinx_ob7_theme'
   html_theme_path = ['_themes']

Then regenerate your documentation::

   make html

And you should see a new theme activated on your html output when opening
it with a web browser.

Advandced Installation as a git submodule:
==========================================

As an alternative to downloading the theme as a zip file, you can use git
submodule support to include it in your git project::

   git submodule add git@github.com:OB7-IRD/sphinx_ob7_theme.git sphinx_ob7_theme
   git submodule
   git submodule init sphinx_ob7_theme
   git submodule update sphinx_ob7_theme
   git status
   git commit -m "Import sphinx_ob7_theme as a git submodule" -a
   git push
   make html

The above being a typical workflow to incorporate the theme as a submodule in
your project.

.. note:: The theme is frozen at the particular version that was current when
   you perform the git submodule update command.

If you wish to obtain updates to the theme submodule, the procedure is
something like this::

   cd <your sphinx project>/sphinx_ob7_theme
   git pull
   cd ..

Then commit the fact that the submodule now tracks a different SHA1::

   git commit -am "Updates sphinx_ob7_theme theme to latest version"

Configuration
=============

This theme is highly customizable on both the page level and on a global level.
To see all the possible configuration options, read the documentation on
`configuring the theme`_.

.. _configuring the theme: https://sphinx-rtd-theme.readthedocs.io/en/latest/configuring.html

Contributing
============

If you would like to help modify or translate the theme, you'll find more
information on contributing in our `contributing guide`_.

.. _contributing guide: https://sphinx-rtd-theme.readthedocs.io/en/latest/contributing.html
