import { Injectable } from '@angular/core';
import { InitOutput } from '../../../assets/pkg/client';
import init from '../../../assets/pkg/client';

export type Response<T> = {
  data: T;
};

@Injectable({
  providedIn: 'platform',
})
export class WasmService {
  public wasm: any;

  constructor() {}

  async loadWasmModule() {
    if (!this.wasm) {
      this.wasm = await import('../../../assets/pkg/client');
      await init({ wasm: this.wasm });
    }
    return this.wasm;
  }
}
