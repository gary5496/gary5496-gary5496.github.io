---
title: Debug Hexo with VS Code
tags: [Node.js,Hexo,Website]
date: 2018-03-21 16:48:30
categories: Node.js
---

### Background

I need to add the code highlighting feature for ABAP language in Hexo, so I would need to debug the Hexo Node.js application. The tool I am using is Visual Studio Code.

The code for programing language is highlighted when the website files are generated from markdown files using Hexo command `hexo generate`. Actually, the hexo command `hexo generate` is the abbreviation for `node <hexo_base_folder>/node_modules/hexo/node_modules/hexo-cli/bin/hexo generate`. So, I will use the configuration `Node.js: Launch Program` in VS code to debug the Hexo application.

### Steps to debug Hexo application

1. Open the Hexo base folder in Visual Studio Code.

2. Switch to `Debug` view, and choose `Add Configuration...` in the `No Configuration` dropdown box

3. In the configuration file `launch.json`, choose `{} Node.js: Launch Program`, change the program parameter for the Hexo bin file location, and add a parameter `args` with value `generate` manually or by pressing `Ctrl + Space`. Save the configuration file.
``` json
{
    // Use IntelliSense to learn about possible attributes.
    // Hover to view descriptions of existing attributes.
    // For more information, visit: https://go.microsoft.com/fwlink/?linkid=830387
    "version": "0.2.0",
    "configurations": [
    {
        "type": "node",
        "request": "launch",
        "name": "node",
        "program": "${workspaceFolder}/node_modules/hexo/node_modules/hexo-cli/bin/hexo",
        "args": [
            "generate"
        ]
    }
    ]
}
```

4. Open a JavaScript file in VS code, like `<hexo base folder>\node_modules\hexo\node_modules\hexo-cli\lib\hexo.js`, and add a breakpoint in it

5. Press the `Start debugging` button to start debugging, and the VS Code debugger will stop at the breakpoint.

   You will find the below information in `DEBUG CONSOLE`.
> node --inspect-brk=25386 node_modules\hexo\node_modules\hexo-cli\bin\hexo generate 
> Debugger listening on ws://127.0.0.1:25386/f253557e-17e7-4480-aa80-88b839ed4fe2
