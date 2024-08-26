import { TestBed } from '@angular/core/testing';
import { CanActivateChildFn } from '@angular/router';

import { studentGuard } from './student.guard';

describe('studentGuard', () => {
  const executeGuard: CanActivateChildFn = (...guardParameters) => 
      TestBed.runInInjectionContext(() => studentGuard(...guardParameters));

  beforeEach(() => {
    TestBed.configureTestingModule({});
  });

  it('should be created', () => {
    expect(executeGuard).toBeTruthy();
  });
});
