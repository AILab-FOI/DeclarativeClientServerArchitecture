import { APP_INITIALIZER, ApplicationConfig } from '@angular/core';
import { provideRouter, withComponentInputBinding } from '@angular/router';

import { routes } from './app.routes';
import { UserService, WasmService } from './core';

export function initializeWasm(
  wasmService: WasmService,
  userService: UserService,
) {
  return () =>
    wasmService.loadWasmModule().then((e) =>
      userService.dohvati_korisnika().then((k) => {
        userService.user.set(k);
      }),
    );
}

export function initializeUser(userService: UserService) {
  return () => userService.dohvati_korisnika();
}

export const appConfig: ApplicationConfig = {
  providers: [
    provideRouter(routes, withComponentInputBinding()),
    WasmService,
    {
      provide: APP_INITIALIZER,
      useFactory: initializeWasm,
      deps: [WasmService, UserService],
      multi: true,
    },
  ],
};
