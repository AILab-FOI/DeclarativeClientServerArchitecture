import { Component, effect, inject, signal } from '@angular/core';
import { dohvati_fakultete } from '../../../assets/pkg/client';
import { Fakultet, TokenService } from '../../core';
import { FacultyCardComponent } from '../../shared/faculty-card/faculty-card.component';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [FacultyCardComponent],
  templateUrl: './home.component.html',
  styleUrl: './home.component.scss',
})
export class HomeComponent {
  public faculties = signal<Fakultet[]>([]);
  constructor() {
    effect(() => {
      dohvati_fakultete().then((fakulteti) => {
        this.faculties.set(fakulteti);
      });
    });
  }
}
