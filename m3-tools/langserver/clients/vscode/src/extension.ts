/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { commands, ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions 
} from 'vscode-languageclient';

let client: LanguageClient;

export function activate(context: ExtensionContext) {

	commands.registerCommand('m3Server.startStreaming', () => {
	});

	// The server is implemented in m3
	const cm3exec = context.asAbsolutePath(
		path.join('bin', 'm3_lang_server')
        );

	const serverOptions: ServerOptions = {
          command : cm3exec,
        };

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'm3' }],
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'm3Server',
		'M3 Language Server',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
