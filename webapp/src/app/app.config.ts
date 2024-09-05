import { APP_INITIALIZER, ApplicationConfig } from '@angular/core';
import {
  provideRouter,
  Router,
  withComponentInputBinding,
} from '@angular/router';
import { provideAnimations } from '@angular/platform-browser/animations';
import { routes } from './app.routes';
import { UserService, WasmService } from './core';
import { TokenService } from './core/services/token.service';
import { provideToastr } from 'ngx-toastr';

export function initializeWasm(
  wasmService: WasmService,
  userService: UserService,
  tokenService: TokenService,
  router: Router,
) {
  return () =>
    wasmService.loadWasmModule().then((e) =>
      userService
        .dohvati_korisnika()
        .then((k) => {
          userService.user.set(k);
        })
        .catch((err) => {
          tokenService.removeAccessToken();
          router.navigate(['/login']);
        }),
    );
}

export function initializeUser(userService: UserService) {
  return () => userService.dohvati_korisnika();
}

export const appConfig: ApplicationConfig = {
  providers: [
    provideRouter(routes, withComponentInputBinding()),
    provideAnimations(),
    provideToastr(),
    WasmService,
    {
      provide: APP_INITIALIZER,
      useFactory: initializeWasm,
      deps: [WasmService, UserService, TokenService, Router],
      multi: true,
    },
  ],
};
