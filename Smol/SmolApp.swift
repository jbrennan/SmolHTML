//
//  SmolApp.swift
//  Smol
//
//  Created by Jason Brennan on 2/9/23.
//

import SwiftUI

@main
struct SmolApp: App {
    var body: some Scene {
        WindowGroup {
			BrowserView(controller: PageController())
        }
    }
}
