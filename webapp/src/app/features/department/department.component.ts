import {
  Component,
  computed,
  effect,
  inject,
  input,
  signal,
} from '@angular/core';
import { Katedra, TokenService, UserService } from '../../core';
import { dohvati_katedru } from '../../../assets/pkg/client';
import { Dialog } from '@angular/cdk/dialog';
import {
  CourseCardComponent,
  openModal,
  UserCardComponent,
} from '../../shared';
import { DepartmentEditComponent } from '../../shared/department-edit/department-edit.component';

@Component({
  selector: 'app-department',
  standalone: true,
  imports: [CourseCardComponent, UserCardComponent],
  templateUrl: './department.component.html',
  styleUrl: './department.component.scss',
})
export class DepartmentComponent {
  public tokenService = inject(TokenService);
  public userService = inject(UserService);
  public id = input.required<string>();
  public department = signal<Katedra>(undefined);
  public isUserLeader = computed(() => {
    if (this.tokenService.exists()) {
      if (
        this.userService.user().uloga === 'Djelatnik' &&
        this.department().djelatnici.find(
          (k) => k.id === this.userService.user().id,
        ).tip !== 'voditelj'
      ) {
        return true;
      }
    }
    return false;
  });
  private dialog = inject(Dialog);
  constructor() {
    effect(() => {
      dohvati_katedru(parseInt(this.id())).then((department) => {
        console.log(department);
        this.department.set(department);
      });
    });
  }

  openEdit(): void {
    let ref = openModal<Katedra>(this.dialog, {
      data: structuredClone(this.userService.user()),
      tool: { view: DepartmentEditComponent },
      inputs: {},
      title: 'Edit section',
    });
    ref.componentInstance['query'].subscribe((user: Katedra) => {
      if (user) {
        console.log(user);
        // TODO: UREĐIVANJE SEKCIJE QUERY
      }
      ref.close();
    });
  }
}
