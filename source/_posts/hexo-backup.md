---
title: Backup Hexo using GitHub
tags: [Website,Git]
date: 2018-03-20 22:39:19
categories: Website
---
 
### Assumption

You have already installed Git from <https://git-scm.com/>, installed Node.js, set up Hexo for your website, created an account in GitHub and created a repository there. 

### Requirement

You want to backup Hexo website data and restore it from anothter computer.

### GitHub

Log on GitHub with your account. Create a branch such as `backup` for repository <account_name>.github.io

### Backup

- Open the file `.gitignore` in Hexo base folder, and check if it contains the below two entries. If no, add them to the file.
```
/.deploy_git
/node_modules
```

  Directory `/.deploy_git` contains the files after generation by command `hexo generate`. We don't need to backup them.
  Directory `/node_modules` contains the node.js modules. We don't need to backup them.

- Execute the below command in the first backup
``` bash
# Init Git
git init
# Create the backup branch, and switch to the new branch
git checkout -b backup
# Link a remote repository to origin
git remote add origin git@github.com:gary5496/gary5496.github.io.git
```

- Execute the below command in each backup
``` bash
# Add the files and sub-directories in the current directory to staging area
git add .
# Commit the changes in the staging area to current branch
git commit -m "commit_comments"
# Push the backup branch to remote repository backup branch
git push origin backup
```

### Restore

If you need to restore the Hexo websites in another computer, you have to follow the [Hexo setup guide](https://gary5496.github.io/2018/03/hexo-github-setup/) to set up the Hexo in another computer first. Then, run the below command the clone the remote repository backup branch to the local directory.

``` bash
# Clone the remote repository backup branch to the current directory
git clone -b backup git@github.com:gary5496/gary5496.github.io.git
```

### Reference

<http://blog.csdn.net/zk673820543/article/details/52698760>
<https://www.liaoxuefeng.com/wiki/0013739516305929606dd18361248578c67b8067c8c017b000>
<http://bbs.itheima.com/forum.php?mod=viewthread&tid=346526&ordertype=1>
