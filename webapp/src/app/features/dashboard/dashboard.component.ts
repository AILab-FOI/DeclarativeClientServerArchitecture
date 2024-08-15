import { Component, effect, inject } from '@angular/core';
import { WasmService } from '../../core';
import { dohvati_studente } from '../../../assets/pkg/client';

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [],
  templateUrl: './dashboard.component.html',
  styleUrl: './dashboard.component.scss',
})
export class DashboardComponent {
  private wasm = inject(WasmService);

  constructor() {
    effect(() => {
      this.wasm
        .callWasmFunction(dohvati_studente(localStorage.getItem('token')))
        .then((response) => {
          console.log(response);
        });
    });
  }
}
