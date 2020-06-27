c.colors.tabs.bar.bg = "#000"
c.colors.tabs.even.bg = "#000"
c.colors.tabs.odd.bg = "#000"
c.colors.tabs.selected.even.bg = "#555"
c.colors.tabs.selected.odd.bg = "#555"
c.completion.open_categories = ["searchengines", "quickmarks", "bookmarks"]
c.completion.shrink = True
c.content.javascript.enabled = False
c.statusbar.padding = {"bottom": 1, "left": 2, "right": 2, "top": 1}
c.statusbar.widgets = ["keypress", "url", "scroll", "tabs", "progress"]
c.tabs.favicons.scale = 0.95
c.tabs.indicator.padding = {"bottom": 0, "left": 0, "right": 4, "top": 0}
c.tabs.mousewheel_switching = False
c.tabs.position = "left"
c.tabs.show = "switching"
c.tabs.title.format = ""
c.tabs.width = 30
c.url.start_pages = ['~/.config/qutebrowser/web/startpage.html']
c.window.hide_decoration = True

config.bind('<Ctrl-m>', 'hint links spawn --detach mpv --force-window yes {hint-url}')

config_dir = str(config.configdir)

# whitelist.txt file goes in same dir as this config file
# with each url on a separate line and NO trailing newline
with open(config_dir + "/js-whitelist.txt") as whitelist:
    urls = whitelist.readlines()
    for url in urls:
        config.set('content.javascript.enabled', True, url.strip())

c.url.searchengines = {
        'DEFAULT' : 'https://duckduckgo.com/html?q={}',
        '4'       : 'https://4chan.org/{}',
        'def'     : 'https://lexico.com/en/definition/{}',
        'eff'     : 'https://eff.org/search/site/{}',
        'etym'    : 'https://etymonline.com/search?q={}',
        'g'       : 'https://google.com/search?q={}',
        'gh'      : 'https://github.com/{}',
        'gl'      : 'https://gitlab.com/{}',
        'hoo'     : 'https://hoogle.haskell.org/?hoogle={}',
        'se'      : 'https://stackexchange.com/search?q={}',
        'r'       : 'https://reddit.com/r/{}',
        'w'       : 'https://en.wikipedia.org/?search={}',
        'wa'      : 'https://wiki.archlinux.org/?search={}',
        'wg'      : 'https://wiki.gentoo.org/?search={}',
        }

