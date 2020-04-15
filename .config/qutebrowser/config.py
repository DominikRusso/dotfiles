c.colors.tabs.bar.bg = "#000"
c.colors.tabs.even.bg = "#000"
c.colors.tabs.odd.bg = "#000"
c.colors.tabs.selected.even.bg = "#555"
c.colors.tabs.selected.odd.bg = "#555"
c.completion.open_categories = ["searchengines", "quickmarks", "bookmarks"]
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
        '4'       : 'https://4chan.org/{}',
        'DEFAULT' : 'https://duckduckgo.com/?q={}',
        'aw'      : 'https://wiki.archlinux.org/?search={}',
        'def'     : 'https://lexico.com/en/definition/{}',
        'etym'    : 'https://etymonline.com/search?q={}',
        'g'       : 'https://google.com/search?q={}',
        'gw'      : 'https://wiki.gentoo.org/search={}',
        'se'      : 'https://stackexchange.com/search?q={}',
        'w'       : 'https://en.wikipedia.org/?search={}',
        }

config.bind('<Ctrl-m>', 'hint links spawn --detach mpv --force-window yes {hint-url}')

