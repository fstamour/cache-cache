# Cache-cache

> caching GitLab issues and more locally, for blazingly fast search

## History and motivation

This project started out as a bash script to fetch all of the issues
from all the projects I had access to at work. I made that script
because GitLab's API to search issues was returning 500 errors for me
(and no one else that I know), which meant the web UI just didn't
work. The issue (ha!) lasted for months. So, with time, I improved
upon my (what I thought would be a one-off) script and I ended up with
this project.

The code is not great (at the moment), but it works very well, and I
still use it almost every day. Even if GitLab's search API now works,
it's much much faster to use this to search a local cache instead of
going through GitLab's API.

2023-07-18 update: I renamed the project from local-gitlab to
cache-cache because I want to add more sources than just GitLab.

## About the name

- This projects is basically a glorified local *cache*.
- It helps me find things that are "hidden" (hard to find).
- "Cache-cache" means "hide and seek" in french.

# Related projects

- [magit's forge](https://github.com/magit/forge)
