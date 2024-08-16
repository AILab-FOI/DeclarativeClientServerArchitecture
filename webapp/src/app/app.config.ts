import { APP_INITIALIZER, ApplicationConfig } from '@angular/core';
import { provideRouter, withComponentInputBinding } from '@angular/router';

import { routes } from './app.routes';
import { WasmService } from './core';

export function initializeWasm(wasmService: WasmService) {
  return () => wasmService.loadWasmModule();
}

export const appConfig: ApplicationConfig = {
  providers: [
    provideRouter(routes, withComponentInputBinding()),
    {
      provide: APP_INITIALIZER,
      useFactory: initializeWasm,
      deps: [WasmService],
      multi: true,
    },
  ],
};
