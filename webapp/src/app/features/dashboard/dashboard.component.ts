import { Component, computed, inject } from '@angular/core';
import { UserService } from '../../core';
import { dohvati_studente } from '../../../assets/pkg/client';

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [],
  templateUrl: './dashboard.component.html',
  styleUrl: './dashboard.component.scss',
})
export class DashboardComponent {
  public user = inject(UserService);
  public username = computed(() => {
    return this.user.user().ime + ' ' + this.user.user().prezime;
  });

  constructor() {
    dohvati_studente(localStorage.getItem('AT')).then((e) => console.log(e));
  }
}
