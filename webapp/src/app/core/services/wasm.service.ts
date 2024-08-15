import { Injectable } from '@angular/core';
import { InitOutput } from '../../../assets/pkg/client';
import init from '../../../assets/pkg/client';

export type Response<T> = {
  data: T;
};

@Injectable({
  providedIn: 'root',
})
export class WasmService {
  private init: Promise<InitOutput>;

  constructor() {
    this.init = init();
  }

  async callWasmFunction<T>(fun: Promise<void>): Promise<Response<T>> {
    return this.init.then(() => fun) as Promise<Response<T>>;
  }
}
