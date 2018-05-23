---
title: GitHub website setup with Hexo in Windows
date: 2018-03-06 14:52:04
tags: [Website]
categories: Website
---
If you want to set up a GitHub website with Hexo in Windows, and you need your website set up with basic functionality, like tags, categories, about, search, theme, visitor count, comments and etc, you can follow the below guide.

## Steps
 
1. Set up the account and repository in GitHub
2. Install Git
3. Install Node.js
4. Install Hexo
5. Hexo basic commands
6. Hexo folder structure and basic setup
7. Change Hexo theme
8. About Markdown
9. Link to a local image
10. Add the pages: tags, categories
11. Add the page: about me
12. Add search functionality
13. Create a new post
14. Deploy to GitHub
15. Count of Visitors
16. Add comments for website
16. Errors & Tips
17. Links & References

### Set up the account and repository in GitHub

Create an account in <https://github.com/>, click on the `Create New` button in the left upper corner, choose `New Repository`. 

In the `Repository name`, input `[your_GitHub_account].github.io` such as "gary5496.github.io", select the checkbox `Initialize this repository with a README`, and click on button `Create repository`.

In the home page of the new repository, choose tab `Settings`. In the `GitHub Pages` section, change `Source` from `None` to `master branch`, and click on `Save` button next to it.


### Install Git

Git is the tool to manage your repository on GitHub.

Download the Git from <https://git-scm.com/download/win>, and install it.

After installation, right click on any folder, and choose `Git Bash Here` to open Git Bash. To bind your GitHub account and email to the Git tool on your local computer, run the below commands.

    git config --global user.name [your GitHub account]
    git config --global user.email [your GitHub email address]

### Install Node.js

Hexo is a framework based on Node.js.

Download Node.js from <https://nodejs.org/en/download/>, and install it. Run the below command to validate the Node.js status after installation.

    node -v

npm is the package manager for JavasSript. Run the below command to check the npm status.

    npm -v

### Install Hexo

Hexo is a framework which is helpful to build a blog. See <https://hexo.io/> for details.

Create a folder as the base folder for Hexo. Run `cmd` command, run `cd` to change the folder to the Hexo base folder, and run the below command to install Hexo.

    npm install -g hexo-cli 

Run the below command to install Hexo Git deployer

    npm install hexo-deployer-git --save

Run the below command to initialize Hexo
    
    hexo init

### Hexo basic commands

Initialize Hexo after installation
    
    hexo init

Start Hexo server. You can stop the server by pressing Ctrl + C.

    hexo server
    hexo s

After the server start, open the below link in browser, and you can see the initial page for Hexo

> http://localhost:4000/

Create a new page, like tags, categories, about me, etc

    hexo new page [page_name]

Create a new post

    hexo new [post_name]
    hexo n [post_name]

Generate the website for .md files
    
    hexo generate
    hexo g

Deploy the website to GitHub

    hexo deploy
    hexo d

Generate the website for .md files and deploy the website to GitHub

    hexo deploy --generate
    hexo d -g

Clean the cache. It may be necessary after the change on a post, a page or a configuration file.

    hexo clean

### Hexo folder structure and basic setup

- node_modules: the folder to save Node.js modules for Hexo
- themes: the folder for themes
- source: the folder to save the source markdown (.md) files
- scaffolds: the folder for templates
- public: the folder to save the HTML pages after generation
- _config.yml: the `root configuration file`
- themes/[theme\_name]/_config.yml: the `theme configuration file`
- source/_posts: the folder to save the post markdown (.md) files

Update the `root configuration file` for the below parameters

``` yaml
url: https://gary5496.github.io/

highlight:
  enable: true
  line_number: true
  auto_detect: true
```

### Change Hexo theme

Check the Hexo themes from <https://hexo.io/themes/>. 

For example, if you want to download the Next theme and use it, follow the below steps.

a. Run the below command to download the Next theme to themes folder.

    git clone https://github.com/theme-next/hexo-theme-next themes/next

b. Change the theme parameter to next in the `root configuration file`
    
``` yaml
theme: next
```

c. Change the parameters in the `theme configuration file` to use another scheme and add social links if necessary.

``` yaml
scheme: Gemini

highlight_theme: night
```

d. Update the file `custom.styl` under folder `\themes\next\source\css\_custom`, and change the parameter as below

    // Custom styles.
    
    code {
      color: #098;
    }

### About Markdown

Markdown is a text-to-HTML formatting syntax for web writers. Markdown allows you to write using an easy-to-read, easy-to-write plain text format, then convert it to structurally valid XHTML (or HTML). See <https://www.appinn.com/markdown/> for Markdown syntax details.

Here is some common syntax.

``` markdown
*italic text*

**bold text**

***

<http://www.url.com>
[url tile](url address)

### header 3

- list item 1
- list item 2

text before `small code block` text after

![Alt text](/path/to/img.jpg)

    code block line 1
    code block line 2
```
    
You can choose a Markdown editor like `MarkdownPad` to edit Markdown files. Download it from <http://www.markdownpad.com/> and install it. The editor has a preview functionality which is very helpful for editing.

Install `Awesomium 1.6.6 SDK` from <http://markdownpad.com/download/awesomium_v1.6.6_sdk_win.exe> and restart the computer, if LivePreview is not working for MarkdownPad and the error message is `This view has crashed!`


### Link to a local image

Update the `root configuration file` as below. After the configuration change, there will be a folder created when using command `hexo new [post_name]` to create a new post, and the folder name will be the same as post name.

``` yaml
post_asset_folder: true
```

You can copy your image file to the post folder, and use the below Markdown syntax to add an image in a post. With this approach, the image will be showing in the post, but it is not showing in the home page.

``` markdown
![alt_text](image_name.jpg)
```

You can install the below package and then use the below Markdown syntax to add an image to a post, and the image will be showing in both the post and home page.

    npm install https://github.com/CodeFalling/hexo-asset-image --save

``` markdown
{% asset_img image_name.jpg This is an image %}
```

### Add the pages: tags, categories

Update the `root configuration file` for the directories.

``` yaml
tag_dir: tags
category_dir: categories
```

Update the `theme configuration file` for the menu parameter.

``` yaml
menu:
  tags: /tags/ || tags
  categories: /categories/ || th
```

Run the below command to create the page for tags

    hexo new page tags

Update the file `\source\tags\index.md`

    ---
    title: tags
    date: 2018-03-06 14:38:05
    type: "tags"
    comments: false
    ---

Run the below command to create the page for categories

    hexo new page categories

Update the file `\source\categories\index.md`

    ---
    title: categories
    date: 2018-03-06 14:39:05
    type: "categories"
    comments: false
    ---

### Add the page: about me

Update the `theme configuration file` for the menu parameter.

``` yaml
menu:
  about: /about/ || user
```

Run the below command to create the page for about

    hexo new page about

Update the file `\source\about\index.md`

    ---
    title: about me
    date: 2018-03-06 14:19:41
    ---
    ### About me
    
    This is my blog.


### Add search functionality

Install the Hexo search package via the below command

    npm install hexo-generator-searchdb --save

Add the below parameters to `root configuration file`.

``` yaml
search:
  path: search.xml
  field: post
```

Change the below parameter in `theme configuration file`.

``` yaml
local_search:
  enable: true
```

### Create a new post

Run the below command to create a new post

    hexo new [post_name]

Update the file `\source\_post\[post_name].md`. See below for example.

``` markdown
---
title: Hello World
date: 2018-03-06 14:19:41
tags: [Windows]
categories: Windows
---
###Hello

Hello! This is my first blog.
    
    printf("Hello world!");
```

### Deploy to GitHub

If you are using `HTTP` to deploy your website to GitHub, you may get a pop-up dialog to input your GitHub account and password when running command `hexo deploy` during deployment.
    

In this case, the `root configuration file` is configured as below.

``` yaml
deploy:
  type: git
  repo: https://github.com/gary5496/gary5496.github.io.git
  name: gary5496
  email: gary5496@163.com
  branch: master
```

If you are using `SSH` to deploy your website to GitHub, you can follow the below steps.

a. Run the below command via Git Bash to generate a key pair.

    ssh-keygen -t rsa -C "your_GitHub_email_address"

b. Use the default file path `C:/User/[Windows_account]/.ssh/`, and input the passphrase if necessary

c. Open the file `C:/User/[Windows_account]/.ssh/id_rsa.pub`, and copy the entire file content

d. Log on to GitHub, click on `Avatar` on the left upper corner, choose `Settings`, choose `SSH and GPG Keys`, click on button `New SSH Key`, input the `Title` with any value, paste the entire file content from last step as `Key`, and click on button `Add SSH Key`

e. If the passphrase is not empty in the step b, and you don't want to reenter your passphrase every time you use your SSH key, you can run the below commands to add your key to the SSH agent, which manages your SSH keys and remembers your passphrase.

    eval $(ssh-agent -s)
    ssh-add ~/.ssh/id_rsa

f. Run the below command via Git Bash to check the SSH connection
    
    ssh -T git@github.com

The successful result should be like below.
    
> Hi gary5496! You've successfully authenticated, but GitHub does not provide shell access.

g. Run the below command to deploy website to GitHub

    hexo deploy

In this case, the `root configuration file` is configured as below.

``` yaml
deploy:
  type: git
  repo: git@github.com:gary5496/gary5496.github.io.git
  branch: master
```

### Count of Visitors

Open the `theme configuration file` and update the parameters as below

``` yaml
busuanzi_count:
  enable: true
  total_visitors: true
  total_visitors_icon: user
  total_views: true
  total_views_icon: eye
  post_views: true
  post_views_icon: eye
```

### Add comments for website

Create a new GitHub repository `website_comments` to save the comments for the website

Request a GitHub `OAuth application` via the link <https://github.com/settings/applications/new>, or via navigation by `GitHub homepage -> Avatar on the left upper corner -> Settings -> Developer Settings -> OAuth Apps -> Request a new application`. Input the parameters as below, and register application

> Application name: website_comments
    
> Homepage URL: https://gary5496.github.io/
    
> Authorization callback URL: https://gary5496.github.io/

Note down the `Client ID` and `Client Secret` on the application page.

Update the `theme configuration file` to add the below parameters

``` yaml
gitalk:
  enable: true
  clientID: [above_client_id]
  clientSecret: [above_client_secret]
  repo: website_comments
  owner: gary5496
  admin: gary5496
  distractionFreeMode: true # Facebook-like distraction free mode
  createIssueManually: false
```

Create a file `gitalk.swig` under folder `next/layout/_third-party/comments`. See below for the file content. 

``` javascript
{% if theme.gitalk.enable %}
  {% if page.comments %}
    <link rel="stylesheet" href="https://unpkg.com/gitalk/dist/gitalk.css">
    <script src="https://unpkg.com/gitalk/dist/gitalk.min.js"></script>
    <script type="text/javascript">
      const gitalk = new Gitalk({
        clientID: '{{theme.gitalk.clientID}}',
        clientSecret: '{{theme.gitalk.clientSecret}}',
        repo: '{{theme.gitalk.repo}}',
        owner: '{{theme.gitalk.owner}}',
        admin: '{{theme.gitalk.admin}}'.split(','),
        id: location.pathname,
        // facebook-like distraction free mode
        distractionFreeMode: '{{ theme.gitalk.distractionFreeMode }}',
        createIssueManually: '{{ theme.gitalk.createIssueManually }}'
      })
      gitalk.render('gitalk-container')
    </script>
  {% endif %}
{% endif %}
```

Update the file `index.swig` under folder `next/layout/_third-party/comments` to add the below code

``` javascript
{% include 'gitalk.swig' %}
```

Update the file `comments.swig` under folder `next/layout/_partials` to add the below code

``` javascript
{% elseif theme.gitalk.distractionFreeMode %}  
  <div id="gitalk-container"></div>
```

### Errors & Tips

Error message:
> After the change on a post, a page or a configuration file, the change is not reflected on the site.

Solution:
> Clean the cache by running the command `hexo clean`


Error message:
> The LivePreview functionality is not working for MarkdownPad editor. The error message is `The view has crashed!`.


Solution:
> Install `Awesomium 1.6.6 SDK` from [this link](http://markdownpad.com/download/awesomium_v1.6.6_sdk_win.exe) and restart the computer

Error message:
> If you are using `SSH` to deploy your Hexo contents to GitHub, you got an error message `Could not read from remote repository` when you are running the command `hexo deploy` to deploy the site to GitHub via cmd.


Solution:
> Use `Git Bash` to run the command, instead of `cmd`


### Links & References

- https://zhuanlan.zhihu.com/p/26625249
- https://codezjx.github.io/2017/07/31/hexo-guide/
- http://theme-next.iissnan.com/theme-settings.html
- http://theme-next.iissnan.com/third-party-services.html#analytics-system
- https://yanyinhong.github.io/2017/05/02/How-to-insert-image-in-hexo-post/
- https://univer2012.github.io/2017/04/23/6how-to-insert-picture-in-hexo-blog/
- https://help.github.com/articles/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent/
- https://www.jianshu.com/p/9be29ed2f4b7
- https://blog.maple3142.net/2017/11/04/hexo-next-readcount/#group-7