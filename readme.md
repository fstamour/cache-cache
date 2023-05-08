# local-gitlab

> caching gitlab issues and more locally, for bazingly fast search



This project started out as a bash script to fetch all of the issues from all the projects I had access to at work. I made that script because GitLab's API to search issues was returning 500 errors for me (and no one else that I know), which meant the web UI just didn't work. The issue (ha!) lasted for months. So, with time, I improved upon my (what I thought would be a one-off) script and I ended up with this project.

The code is not great (at the moment), but it works very well, and I still use it almost every day. Even if GitLab's search API now works, it's much much faster to use this to search a local cache instead of going through GitLab's API.

P.S. You might wonder why it's hosted on GitHub and not GitLab... I might add other sources in time.

# Related projects

- [magit's forge](https://github.com/magit/forge)

