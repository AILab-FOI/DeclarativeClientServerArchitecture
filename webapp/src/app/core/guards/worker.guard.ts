import { inject } from '@angular/core';
import { CanActivateChildFn, CanActivateFn, Router } from '@angular/router';
import { UserService } from '../services/user.service';

export const workerGuard: CanActivateFn = (route, state) => {
  let user = inject(UserService);
  let router = inject(Router);
  if (user.user().uloga === 'Djelatnik') return true;
  router.navigate(['dashboard']);
  return false;
};
