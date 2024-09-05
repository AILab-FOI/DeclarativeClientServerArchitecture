import {
  afterNextRender,
  AfterViewInit,
  Component,
  effect,
  ElementRef,
  inject,
  input,
  signal,
  viewChild,
} from '@angular/core';
import { Fakultet, TokenService, UserService } from '../../core';
import { dohvati_fakultet } from '../../../assets/pkg/client';
import { DepartmentCardComponent } from '../../shared/department-card/department-card.component';

import { CommonModule } from '@angular/common';
import { MapComponent } from '../../shared/map/map.component';
import { openModal } from '../../shared';
import { Dialog } from '@angular/cdk/dialog';
import { FacultyEditComponent } from '../../shared/faculty-edit/faculty-edit.component';

@Component({
  selector: 'app-faculty',
  standalone: true,
  imports: [CommonModule, DepartmentCardComponent, MapComponent],
  templateUrl: './faculty.component.html',
  styleUrl: './faculty.component.scss',
})
export class FacultyComponent {
  public tokenService = inject(TokenService);
  public userService = inject(UserService);
  public id = input.required<string>();
  public faculty = signal<Fakultet>(undefined);
  private dialog = inject(Dialog);
  constructor() {
    effect(() => {
      dohvati_fakultet(parseInt(this.id())).then((fakultet) => {
        this.faculty.set(fakultet);
      });
    });
  }
  openEdit(): void {
    let ref = openModal<Fakultet>(this.dialog, {
      data: structuredClone(this.userService.user()),
      tool: { view: FacultyEditComponent },
      inputs: {},
      title: 'Edit section',
    });
    ref.componentInstance['query'].subscribe((user: Fakultet) => {
      if (user) {
        console.log(user);
        // TODO: UREĐIVANJE SEKCIJE QUERY
      }
      ref.close();
    });
  }
}
