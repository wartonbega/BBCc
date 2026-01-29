import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
    console.log('BBC Language extension is now active');

    // Get LSP path from configuration or use default
    const config = vscode.workspace.getConfiguration('bbc');
    let serverPath = config.get<string>('lsp.path');

    if (!serverPath || serverPath === '') {
        // Try to find the LSP server in common locations
        const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
        if (workspaceRoot) {
            serverPath = path.join(workspaceRoot, 'zig-out', 'bin', 'bbc-lsp');
        } else {
            vscode.window.showErrorMessage('BBC LSP: Cannot find bbc-lsp executable. Please set bbc.lsp.path in settings.');
            return;
        }
    }

    console.log(`Using LSP server at: ${serverPath}`);

    const serverOptions: ServerOptions = {
        command: serverPath,
        args: [],
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'bbc' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.bbc')
        }
    };

    client = new LanguageClient(
        'bbcLanguageServer',
        'BBC Language Server',
        serverOptions,
        clientOptions
    );

    client.start().then(() => {
        console.log('BBC Language Server started successfully');
        vscode.window.showInformationMessage('BBC LSP connected!');

        // Manually notify LSP about already-open .bbc files
        if (client) {
            vscode.workspace.textDocuments.forEach(document => {
                if (document.languageId === 'bbc') {
                    console.log(`Manually opening already-open document: ${document.uri.toString()}`);
                    // Send a manual didOpen notification
                    client!.sendNotification('textDocument/didOpen', {
                        textDocument: {
                            uri: document.uri.toString(),
                            languageId: document.languageId,
                            version: 1,
                            text: document.getText()
                        }
                    });
                }
            });
        }
    }).catch((error) => {
        console.error('Failed to start BBC Language Server:', error);
        vscode.window.showErrorMessage(`Failed to start BBC LSP: ${error.message}\nCheck console for details`);
    });

    console.log('Language client starting...');
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}