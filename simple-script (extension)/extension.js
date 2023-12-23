const vscode = require('vscode');

let terminal; // Declare the terminal variable outside the activate function

function activate(context) {
    let disposable = vscode.commands.registerCommand('extension.runMyLanguage', function () {
        // The code you want to run when your command is executed

        // Get the current file name
        let fileName = vscode.window.activeTextEditor ? vscode.window.activeTextEditor.document.fileName : '';
        if (!fileName) {
            vscode.window.showErrorMessage('No active file');
            return;
        }

        // Check if the terminal already exists, if not create a new one
        if (!terminal) {
            terminal = vscode.window.createTerminal(`Script`);
        }

        // Run your language's interpreter
        terminal.sendText(`sc ${fileName}`);
        terminal.show();
    });

    context.subscriptions.push(disposable);
}

exports.activate = activate;