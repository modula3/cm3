"use strict";
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const path = require("path");
const vscode_1 = require("vscode");
const vscode_languageclient_1 = require("vscode-languageclient");
let client;
function activate(context) {
    vscode_1.commands.registerCommand('m3Server.startStreaming', () => {
    });
    // The server is implemented in m3
    const cm3exec = context.asAbsolutePath(path.join('bin', 'm3_lang_server'));
    const serverOptions = {
        command: cm3exec,
    };
    // Options to control the language client
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'm3' }],
    };
    // Create the language client and start the client.
    client = new vscode_languageclient_1.LanguageClient('m3Server', 'M3 Language Server', serverOptions, clientOptions);
    // Start the client. This will also launch the server
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map