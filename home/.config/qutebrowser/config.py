config.load_autoconfig(False) # don't read autoconfig.yaml

config_dir = str(config.configdir)

c.colors.tabs.bar.bg = '#000'
c.colors.tabs.even.bg = '#000'
c.colors.tabs.odd.bg = '#000'
c.colors.tabs.selected.even.bg = '#555'
c.colors.tabs.selected.odd.bg = '#555'
c.colors.webpage.preferred_color_scheme = 'dark'
c.completion.height = '100%'
c.completion.open_categories = ['searchengines', 'quickmarks', 'bookmarks']
c.completion.shrink = True
c.content.autoplay = False
c.content.canvas_reading = False
c.content.javascript.enabled = False
c.content.notifications.enabled = False
c.downloads.location.directory = '~/downloads'
c.downloads.location.prompt = False
c.downloads.position = 'bottom'
c.editor.command = ['alacritty', '-e', 'nvim', '{}']
c.hints.chars = 'abcdefghijklmnopqrstuvwxyz'
c.logging.level.console = 'warning'
c.logging.level.ram = 'warning'
c.scrolling.bar = 'never'
c.statusbar.padding = {'bottom': 1, 'left': 2, 'right': 2, 'top': 1}
c.statusbar.widgets = ['keypress', 'url', 'scroll', 'progress']
c.tabs.favicons.scale = 0.95
c.tabs.indicator.width = 0
c.tabs.mousewheel_switching = False
c.tabs.position = 'left'
c.tabs.show = 'multiple'
c.tabs.title.format = ''
c.tabs.width = 25
c.url.start_pages = [config_dir + '/web/startpage.html']
c.window.hide_decoration = True

# <Space> as the leader key
config.bind('<Space>m', 'hint links spawn --detach mpv --force-window=immediate {hint-url}')

# navigation in command mode using vim keys (like in fzf)
config.bind('<Ctrl-j>', 'completion-item-focus next', mode='command')
config.bind('<Ctrl-k>', 'completion-item-focus prev', mode='command')

# `js-whitelist` file goes in same dir as this config file
# with each url on a separate line and NO trailing newlines
with open(config_dir + '/js-whitelist') as whitelist:
    urls = whitelist.readlines()
    for url in urls:
        config.set('content.javascript.enabled', True, url.strip())

# `searchengines` files goes in same dir as this config file
# with key and value separated by one space and NO trailing newlines
with open(config_dir + '/searchengines') as searchengines:
    dict = {}
    for line in searchengines:
        (key, val) = line.split()
        dict[key] = val
    c.url.searchengines = dict
