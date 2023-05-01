const { app, BrowserWindow, globalShortcut, shell } = require('electron')

const title = "Local GitLab";

let win = null;

const createWindow = () => {
    win = new BrowserWindow({
        title,
        // I'm using a tiling window manager, so I have no idea if
        // those dimensions are good.
        width: 800,
        height: 600
    });


    win.on('page-title-updated', function(e, title, explicitSet) {
        console.log(e, title, explicitSet);
        e.preventDefault()
    });

    win.setMenuBarVisibility(false);

    win.webContents.setWindowOpenHandler(({ url }) => {
        shell.openExternal(url);
        return { action: 'deny' };
    });

    // win.on('focus', () => {
    // });

    win
        .loadURL('http://localhost:40000')
        .then(() => {
            win.title = title;
        });
}


// Quit the application if all windows are closed (Windows & linux)
app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') {
        app.quit();
    }
})

// Open a window if none are open (macOS)
app.whenReady().then(() => {
    // https://www.electronjs.org/docs/latest/api/accelerator
    globalShortcut.register('num1', () => {
        win.show();
        win.reload(); // very stupid way to give the focus to the text input
    })

    createWindow();

    app.on('activate', () => {
        if (BrowserWindow.getAllWindows().length === 0)  {
            createWindow();
        }
    });
});
