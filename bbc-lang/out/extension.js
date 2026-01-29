"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const path = __importStar(require("path"));
const vscode = __importStar(require("vscode"));
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    console.log('BBC Language extension is now active');
    // Get LSP path from configuration or use default
    const config = vscode.workspace.getConfiguration('bbc');
    let serverPath = config.get('lsp.path');
    if (!serverPath || serverPath === '') {
        // Try to find the LSP server in common locations
        const workspaceRoot = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
        if (workspaceRoot) {
            serverPath = path.join(workspaceRoot, 'zig-out', 'bin', 'bbc-lsp');
        }
        else {
            vscode.window.showErrorMessage('BBC LSP: Cannot find bbc-lsp executable. Please set bbc.lsp.path in settings.');
            return;
        }
    }
    console.log(`Using LSP server at: ${serverPath}`);
    const serverOptions = {
        command: serverPath,
        args: [],
    };
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'bbc' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.bbc')
        }
    };
    client = new node_1.LanguageClient('bbcLanguageServer', 'BBC Language Server', serverOptions, clientOptions);
    client.start().then(() => {
        console.log('BBC Language Server started successfully');
        vscode.window.showInformationMessage('BBC LSP connected!');
        // Manually notify LSP about already-open .bbc files
        if (client) {
            vscode.workspace.textDocuments.forEach(document => {
                if (document.languageId === 'bbc') {
                    console.log(`Manually opening already-open document: ${document.uri.toString()}`);
                    // Send a manual didOpen notification
                    client.sendNotification('textDocument/didOpen', {
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
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
//# sourceMappingURL=extension.js.map