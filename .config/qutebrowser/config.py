c.colors.tabs.bar.bg = "#000"
c.colors.tabs.even.bg = "#000"
c.colors.tabs.odd.bg = "#111"
c.colors.tabs.selected.even.bg = "#555"
c.colors.tabs.selected.odd.bg = "#555"
c.completion.shrink = True
c.statusbar.padding = {"bottom": 1, "left": 2, "right": 2, "top": 1}
c.statusbar.widgets = ["keypress", "url", "scroll", "tabs", "progress"]
c.tabs.favicons.scale = 0.95
c.tabs.indicator.padding = {"bottom": 0, "left": 0, "right": 4, "top": 0}
c.tabs.position = "left"
c.tabs.show = "switching"
c.tabs.title.format = ""
c.tabs.width = 30
c.url.start_pages = ['~/.config/qutebrowser/web/startpage.html']
c.window.hide_decoration = True

c.tabs.mousewheel_switching = False
c.url.searchengines = {
        'DEFAULT' : 'https://duckduckgo.com/?q={}',
        '4'       : 'https://4chan.org/{}',
        'aw'      : 'https://wiki.archlinux.org/?search={}',
        'def'     : 'https://lexico.com/en/definition/{}',
        'etym'    : 'https://etymonline.com/search?q={}',
        'g'       : 'https://google.com/?q={}',
        'gw'      : 'https://wiki.gentoo.org/search={}',
        'w'       : 'https://en.wikipedia.org/?search)={}',
        'wd'      : 'https://en.wiktionary.org/wiki/{}'
        }

config.bind('<Ctrl-Shift-m>', 'hint links spawn --detach mpv --force-window yes {hint-url}')
