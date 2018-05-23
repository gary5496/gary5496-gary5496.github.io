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
