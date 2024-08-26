import { TestBed } from '@angular/core/testing';
import { CanActivateChildFn } from '@angular/router';

import { workerGuard } from './worker.guard';

describe('workerGuard', () => {
  const executeGuard: CanActivateChildFn = (...guardParameters) => 
      TestBed.runInInjectionContext(() => workerGuard(...guardParameters));

  beforeEach(() => {
    TestBed.configureTestingModule({});
  });

  it('should be created', () => {
    expect(executeGuard).toBeTruthy();
  });
});
